&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File              : r-poprt.w

  Description       : PO Printing

  Input Parameters  : <none>

  Output Parameters : <none>

  Author            : JLF

  Created           : 08/05/02

  History           : dgd 06/21/2007  - Task# 06200703 - Vendor Specific Forms
                                        via N-K-1

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
DEFINE VARIABLE list-name AS cha NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
 cocode = gcompany
 locode = gloc.

DEFINE VARIABLE v-program         AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form    AS LOG  NO-UNDO.
DEFINE VARIABLE ls-fax-file       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-multi-faxout   AS LOG  NO-UNDO.
DEFINE VARIABLE lv-fax-image      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-pdf-file       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-exp-form-list  AS CHARACTER NO-UNDO INIT "CorrTrim,Alliance,HRMS,CorSuply,Corr-U-KraftII,GP,Kiwi,Smurfit,CorrChoice,Pratt,AlliFlutes,iPaper,Kiwit,Liberty".
DEFINE VARIABLE lv-exp-prog-list  AS CHARACTER NO-UNDO INIT "po-ctexp,po-alexp,po-hrexp,po-csexp,po-ckexp,po-gpexp,po-kwexp,po-smurfi,po-ccexp,po-prexp,~
po-alnceexp,po-ipaper,po-ktexp,po-librt".
DEFINE VARIABLE vcDefaultForm     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-fax-type       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-attachments    AS LOG  NO-UNDO.
DEFINE TEMP-TABLE w-export NO-UNDO
    FIELD w-exp-prog   AS CHARACTER.

{po/po-print.i NEW}
{custom/xprint.i}
DEFINE NEW SHARED VARIABLE s-group-notes AS LOG NO-UNDO.
DEFINE NEW SHARED VARIABLE s-print-prices AS LOG NO-UNDO.
/* Variables defined for Excel */
/* Build a Table to keep sequence of pdf files */

DEFINE NEW SHARED 
       TEMP-TABLE tt-filelist
            FIELD tt-FileCtr  AS INTEGER
            FIELD tt-FileName AS CHARACTER
            INDEX filelist    IS PRIMARY TT-FILECTR.

/* Output selection for the report */
DEFINE NEW SHARED VARIABLE LvOutputSelection AS CHARACTER NO-UNDO.

DO TRANSACTION:
  {sys/inc/poexport.i}
  {sys/inc/poimage.i}
END.

DEFINE BUFFER b1-po-ord   FOR po-ord.

/* gdm - 11190804 */
DEFINE BUFFER bf-attach FOR attach.

/* Variables */
DEFINE VARIABLE vcErrorMsg AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE llRecFound AS LOG NO-UNDO.
DEFINE VARIABLE poPaperClip-log AS LOG NO-UNDO.
RUN sys/ref/nk1look.p (cocode, "POPaperClip", "L", NO, NO, "", "", 
                          OUTPUT cRtnChar, OUTPUT llRecFound).
IF llRecFound THEN
        poPaperClip-log = LOGICAL(cRtnChar).

DEFINE VARIABLE retcode AS INTEGER   NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.

 RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_po-no end_po-no ~
begin_vend-no end_vend-no tb_reprint tb_reprint-closed tb_delete ~
tb_print-terms tb_spec tb_attachments tb_cust-code tb_corr tb_group-notes ~
tb_Summarize-by-item tb_itemDescription tb_score-types tb_metric ~
tb_print-prices rd-dest lv-ornt lines-per-page lv-font-no td-show-parm ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_po-no end_po-no begin_vend-no ~
end_vend-no tb_reprint tb_reprint-closed tb_delete tb_print-terms tb_spec ~
tb_attachments tb_cust-code tb_corr tb_group-notes tb_Summarize-by-item ~
tb_itemDescription tb_score-types tb_metric tb_print-prices rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AttachmentExists C-Win 
FUNCTION AttachmentExists RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

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

DEFINE VARIABLE begin_po-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning PO#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending PO#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend-no AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 80 column Report)" 
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
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 8.33.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 13.33.

DEFINE VARIABLE tb_attachments AS LOGICAL INITIAL no 
     LABEL "Email Attachments" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE tb_corr AS LOGICAL INITIAL no 
     LABEL "Transfer to Corrugator?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE tb_cust-code AS LOGICAL INITIAL no 
     LABEL "Print Customer Code for each PO Line?" 
     VIEW-AS TOGGLE-BOX
     SIZE 41.8 BY .81 NO-UNDO.

DEFINE VARIABLE tb_delete AS LOGICAL INITIAL no 
     LABEL "Do you want to print deleted line items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .81 NO-UNDO.

DEFINE VARIABLE tb_group-notes AS LOGICAL INITIAL no 
     LABEL "Group Notes on Same Page?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE tb_itemDescription AS LOGICAL INITIAL no 
     LABEL "Print FG Item Description 3 Line?" 
     VIEW-AS TOGGLE-BOX
     SIZE 39.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_metric AS LOGICAL INITIAL no 
     LABEL "Print Metric?" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-prices AS LOGICAL INITIAL no 
     LABEL "Print Prices?" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-terms AS LOGICAL INITIAL no 
     LABEL "Print Terms and Conditions?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE tb_reprint AS LOGICAL INITIAL no 
     LABEL "Do you want to reprint the PO's?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE tb_reprint-closed AS LOGICAL INITIAL no 
     LABEL "Do you want to reprint closed PO's?" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81 NO-UNDO.

DEFINE VARIABLE tb_score-types AS LOGICAL INITIAL no 
     LABEL "Print Score Types?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE tb_spec AS LOGICAL INITIAL yes 
     LABEL "Print Specification Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE tb_Summarize-by-item AS LOGICAL INITIAL no 
     LABEL "Summarize by Item Code/Job?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_po-no AT ROW 2.91 COL 28 COLON-ALIGNED HELP
          "Enter Beginning PO Number"
     end_po-no AT ROW 2.91 COL 69 COLON-ALIGNED HELP
          "Enter Ending PO number"
     begin_vend-no AT ROW 3.86 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend-no AT ROW 3.86 COL 69 COLON-ALIGNED HELP
          "Enter Ending Vendor number"
     tb_reprint AT ROW 5.05 COL 10.6
     tb_reprint-closed AT ROW 5.05 COL 53
     tb_delete AT ROW 6 COL 10.6
     tb_print-terms AT ROW 6 COL 53
     tb_spec AT ROW 6.95 COL 10.6
     tb_attachments AT ROW 6.95 COL 53 WIDGET-ID 2
     tb_cust-code AT ROW 7.86 COL 53 WIDGET-ID 4
     tb_corr AT ROW 7.91 COL 10.6
     tb_group-notes AT ROW 8.86 COL 10.6
     tb_Summarize-by-item AT ROW 9.81 COL 10.6
     tb_itemDescription AT ROW 10.76 COL 10.6
     tb_score-types AT ROW 11.71 COL 10.6
     tb_metric AT ROW 12.67 COL 10.6
     tb_print-prices AT ROW 13.62 COL 10.6
     rd-dest AT ROW 15.76 COL 6 NO-LABEL
     lv-ornt AT ROW 16 COL 31 NO-LABEL
     lines-per-page AT ROW 16 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 17.91 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 18.86 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 21.48 COL 31
     btn-ok AT ROW 23.62 COL 22
     btn-cancel AT ROW 23.62 COL 61
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 14.81 COL 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.48 COL 6
          BGCOLOR 2 
     RECT-6 AT ROW 14.57 COL 1
     RECT-7 AT ROW 1.24 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.4 BY 24.48.


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
         TITLE              = "Print Purchase Orders"
         HEIGHT             = 24.48
         WIDTH              = 95.2
         MAX-HEIGHT         = 46.48
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 46.48
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
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN 
       begin_po-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_po-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_attachments:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_corr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_delete:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_group-notes:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_itemDescription:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_metric:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_print-prices:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_print-terms:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_reprint:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_reprint-closed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_score-types:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_spec:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_Summarize-by-item:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Print Purchase Orders */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Print Purchase Orders */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-no C-Win
ON HELP OF begin_po-no IN FRAME FRAME-A /* Beginning PO# */
DO:
  RUN help-po-no (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-no C-Win
ON LEAVE OF begin_po-no IN FRAME FRAME-A /* Beginning PO# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend-no C-Win
ON LEAVE OF begin_vend-no IN FRAME FRAME-A /* Beginning Vendor# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  SESSION:SET-WAIT-STATE ("general").

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  /* SKB - 1/24/07 - Changes for XL printing */

  IF      rd-dest = 1  THEN ASSIGN LvOutputSelection = "Printer".
  ELSE IF rd-dest = 2  THEN ASSIGN LvOutputSelection = "Screen". 
  ELSE IF rd-dest = 3  THEN ASSIGN LvOutputSelection = "File". 
  ELSE IF rd-dest = 4  THEN ASSIGN LvOutputSelection = "Fax". 
  ELSE IF rd-dest = 5  THEN ASSIGN LvOutputSelection = "Email".
  ELSE IF rd-dest = 6  THEN ASSIGN LvOutputSelection = "Port".

  ASSIGN
    v-start-po          = begin_po-no
    v-end-po            = end_po-no 
    v-reprint-po        = tb_reprint
    v-printde-po        = tb_delete
    v-print-sn          = tb_spec
    v-corrugator        = tb_corr
    v-sendfax           = NO
    v-faxprog           = ""
    v-tmp-fax           = ""
    s-group-notes       = tb_group-notes
    v-summarize-by-item = tb_summarize-by-item
    v-itemDescription   = tb_itemDescription
    v-score-types       = tb_score-types
    v-metric            = tb_metric
    s-print-prices      = tb_print-prices
    v-print-terms       = tb_print-terms
    lv-attachments      = tb_attachments
    lCustCode           =  tb_cust-code.

  IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
     sys-ctrl-shipto.company = cocode AND
     sys-ctrl-shipto.NAME = "POPRINT") THEN
     DO:
        IF CAN-FIND(FIRST b1-po-ord
            WHERE  b1-po-ord.company EQ cocode
              AND (b1-po-ord.stat    EQ "N" OR 
                   b1-po-ord.stat    EQ "O" OR 
                   b1-po-ord.stat    EQ "U" OR
                  (tb_reprint-closed AND b1-po-ord.stat EQ "C"))
              AND  b1-po-ord.printed EQ v-reprint-po
              AND  b1-po-ord.po-no   GE v-start-po
              AND  b1-po-ord.po-no   LE v-end-po
              AND  b1-po-ord.vend-no GE begin_vend-no
              AND  b1-po-ord.vend-no LE end_vend-no) THEN
            FOR EACH  b1-po-ord /* FIELDS(vend-no company) */
                WHERE  b1-po-ord.company EQ cocode
                  AND (b1-po-ord.stat    EQ "N" OR 
                       b1-po-ord.stat    EQ "O" OR 
                       b1-po-ord.stat    EQ "U" OR
                      (tb_reprint-closed AND b1-po-ord.stat EQ "C"))
                  AND  b1-po-ord.printed EQ v-reprint-po
                  AND  b1-po-ord.po-no   GE v-start-po
                  AND  b1-po-ord.po-no   LE v-end-po
                  AND  b1-po-ord.vend-no GE begin_vend-no
                  AND  b1-po-ord.vend-no LE end_vend-no
              NO-LOCK
             BREAK BY b1-po-ord.company
                   BY b1-po-ord.vend-no:

               IF FIRST-OF (b1-po-ord.vend-no) THEN DO:

                 FIND FIRST sys-ctrl-shipto
                      WHERE sys-ctrl-shipto.company      = cocode
                        AND sys-ctrl-shipto.NAME         = "POPRINT"
                        AND sys-ctrl-shipto.cust-vend    = NO
                        AND sys-ctrl-shipto.cust-vend-no = b1-po-ord.vend-no 
                        AND sys-ctrl-shipto.char-fld > '' 
                      NO-LOCK NO-ERROR.

                 IF AVAILABLE sys-ctrl-shipto THEN
                 DO:
                    RUN SetPOPrintForm (sys-ctrl-shipto.char-fld) .
                    v-print-fmt = sys-ctrl-shipto.char-fld.
                 END.
                 ELSE
                 DO:
                    RUN SetPOPrintForm (vcDefaultForm).
                    v-print-fmt = vcDefaultForm.
                 END.

                 RUN SetGlobalVariables(INPUT b1-po-ord.po-no).

                 RUN run-report(b1-po-ord.vend-no, TRUE) . 

                 RUN GenerateReport(b1-po-ord.vend-no, b1-po-ord.vend-no) .
               END.
            END. /*FOR EACH*/
         ELSE
            MESSAGE "No Purchase Orders Were Printed."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
     END.
  ELSE
     DO:
        v-print-fmt = vcDefaultForm.
        RUN SetGlobalVariables(INPUT begin_po-no).
        RUN run-report("", FALSE) .
        RUN GenerateReport(INPUT begin_vend-no, end_vend-no) .
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-no C-Win
ON HELP OF end_po-no IN FRAME FRAME-A /* Ending PO# */
DO:
  RUN help-po-no (2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-no C-Win
ON LEAVE OF end_po-no IN FRAME FRAME-A /* Ending PO# */
DO:
  ASSIGN {&self-name}.
   IF begin_po-no = END_po-no THEN DO:
     FIND FIRST po-ord WHERE po-ord.company = g_company
                         AND po-ord.po-no  = begin_po-no NO-LOCK NO-ERROR.
     IF AVAILABLE po-ord THEN ASSIGN begin_vend-no:SCREEN-VALUE = po-ord.vend-no
                                 end_vend-no:SCREEN-VALUE = po-ord.vend-no.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend-no C-Win
ON LEAVE OF end_vend-no IN FRAME FRAME-A /* Ending Vendor# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
    DEFINE VARIABLE char-val AS cha NO-UNDO.

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
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_corr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_corr C-Win
ON VALUE-CHANGED OF tb_corr IN FRAME FRAME-A /* Transfer to Corrugator? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cust-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-code C-Win
ON VALUE-CHANGED OF tb_cust-code IN FRAME FRAME-A /* Print Customer Code for each PO Line? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_delete C-Win
ON VALUE-CHANGED OF tb_delete IN FRAME FRAME-A /* Do you want to print deleted line items? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_group-notes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_group-notes C-Win
ON VALUE-CHANGED OF tb_group-notes IN FRAME FRAME-A /* Group Notes on Same Page? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_itemDescription
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_itemDescription C-Win
ON VALUE-CHANGED OF tb_itemDescription IN FRAME FRAME-A /* Print FG Item Description 3 Line? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_metric
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_metric C-Win
ON VALUE-CHANGED OF tb_metric IN FRAME FRAME-A /* Print Metric? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_print-prices
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_print-prices C-Win
ON VALUE-CHANGED OF tb_print-prices IN FRAME FRAME-A /* Print Prices? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_print-terms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_print-terms C-Win
ON VALUE-CHANGED OF tb_print-terms IN FRAME FRAME-A /* Print Terms and Conditions? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_reprint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_reprint C-Win
ON VALUE-CHANGED OF tb_reprint IN FRAME FRAME-A /* Do you want to reprint the PO's? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_reprint-closed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_reprint-closed C-Win
ON VALUE-CHANGED OF tb_reprint-closed IN FRAME FRAME-A /* Do you want to reprint closed PO's? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_score-types
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_score-types C-Win
ON VALUE-CHANGED OF tb_score-types IN FRAME FRAME-A /* Print Score Types? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_spec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_spec C-Win
ON VALUE-CHANGED OF tb_spec IN FRAME FRAME-A /* Print Specification Notes? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_Summarize-by-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_Summarize-by-item C-Win
ON VALUE-CHANGED OF tb_Summarize-by-item IN FRAME FRAME-A /* Summarize by Item Code/Job? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE li-lineperpage AS INTEGER NO-UNDO.

SESSION:DATA-ENTRY-RETURN = YES.
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
/* (NOTE: HANDLE ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

 /* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "POPRINT"
      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE sys-ctrl THEN DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "POPRINT"
     sys-ctrl.descrip  = "Print Sheet Size?   16th's or 32nd's?"
     sys-ctrl.char-fld = "32nd's"
     sys-ctrl.log-fld  = NO.
    MESSAGE "System control record NOT found. " "Print Sheet Size? "
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
    MESSAGE "16th's or 32nd's display? " UPDATE sys-ctrl.char-fld.
  END.
  ASSIGN
   v-print-fmt = sys-ctrl.char-fld
   v-shtsiz    = sys-ctrl.log-fld.

  FIND FIRST po-ctrl WHERE po-ctrl.company EQ cocode NO-LOCK NO-ERROR.
  ASSIGN
   v-pre-printed-forms = po-ctrl.pre-printed-forms
   v-company           = po-ctrl.prcom
   vcDefaultForm = v-print-fmt.

  FIND FIRST users WHERE
       users.user_id EQ USERID("NOSWEAT")
       NO-LOCK NO-ERROR.

  IF AVAILABLE users AND users.user_program[2] NE "" THEN
     init-dir = users.user_program[2].
  ELSE
     init-dir = "c:\tmp".

  RUN SetPOPrintForm (v-print-fmt).

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    DISABLE lines-per-page.
    ASSIGN
     lines-per-page              = li-lineperpage
     lines-per-page:SCREEN-VALUE = STRING(li-lineperpage).

    IF NOT CAN-DO('Brick,CSC,Southpak,Xprint,poprint 1,poprint 10,LancoYork,StClair,Boss,PeachTree,Asixprnt,PPI,CSC-GA,Indiana,Packrite,Allwest,Bell,ACPI,Sultana,CCC,Protagon,SouleMed,Soule,Hughes',v-print-fmt) THEN DISABLE tb_spec.

    IF NOT CAN-DO('Xprint,poprint 1,poprint 10,LancoYork,StClair,Boss,Hughes,PeachTree,Protagon,PPI,Packrite,Sultana',v-print-fmt) THEN DO:
       IF v-print-fmt NE "CentBox" THEN
          ASSIGN
             tb_itemDescription = NO
             tb_itemDescription:SCREEN-VALUE = 'NO'

         tb_score-types              = CAN-DO("Premierx,PremierCX,PremierXFGItems,Fibrex,Lovepac,POPrint10-CAN,MWFibre,Protagon,Sultana",v-print-fmt)
         tb_score-types:SCREEN-VALUE = STRING(tb_score-types)
         tb_score-types:SENSITIVE    = YES.

       DISABLE tb_itemDescription
               tb_score-types.
    END.

    IF v-print-fmt EQ "CSC" THEN
       ASSIGN
          tb_score-types:SCREEN-VALUE = STRING(tb_score-types)
          tb_score-types:SENSITIVE = YES.


    IF v-print-fmt EQ "CentBox" THEN
       ASSIGN
          tb_itemDescription:LABEL = "Print P.O. Description Lines"
          tb_itemDescription:SENSITIVE = YES.

    IF LOOKUP(v-print-fmt,"xprint,poprint 1,poprint 10,LancoYork,StClair,Boss,Hughes,PeachTree,Protagon,ppi,Packrite") = 0 THEN 
       DISABLE tb_metric.

   /* IF v-print-fmt NE "Indiana" OR v-print-fmt NE "Indiana" THEN*/
        IF LOOKUP(v-print-fmt,"Indiana,Premierx,PremierCX,PremierXFGItems") = 0 THEN
            ASSIGN tb_print-prices:SCREEN-VALUE = "NO"
                   tb_print-prices:SENSITIVE = NO.
   
    IF LOOKUP(v-print-fmt,"poprint 10,poprint 20,POPrint10-CAN") = 0 THEN 
       DISABLE tb_cust-code.

    IF NOT poPaperClip-log THEN 
        ASSIGN tb_attachments:SCREEN-VALUE = "NO"
               tb_attachments:SENSITIVE    = NO.

    IF NOT poexport-log THEN DISABLE tb_corr.
    APPLY "entry" TO begin_po-no IN FRAME {&FRAME-NAME}.
  END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-export C-Win 
PROCEDURE create-export :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-export AS CHARACTER NO-UNDO.

  CREATE w-export.
  w-exp-prog = ENTRY(LOOKUP(ip-export,lv-exp-form-list),lv-exp-prog-list).

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
  DISPLAY begin_po-no end_po-no begin_vend-no end_vend-no tb_reprint 
          tb_reprint-closed tb_delete tb_print-terms tb_spec tb_attachments 
          tb_cust-code tb_corr tb_group-notes tb_Summarize-by-item 
          tb_itemDescription tb_score-types tb_metric tb_print-prices rd-dest 
          lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_po-no end_po-no begin_vend-no end_vend-no 
         tb_reprint tb_reprint-closed tb_delete tb_print-terms tb_spec 
         tb_attachments tb_cust-code tb_corr tb_group-notes 
         tb_Summarize-by-item tb_itemDescription tb_score-types tb_metric 
         tb_print-prices rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
         btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateReport C-Win 
PROCEDURE GenerateReport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER ip-begin-vend-no AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-end-vend-no AS CHARACTER NO-UNDO.

  /* gdm - 11190804 */
  DEFINE VARIABLE v-outfile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lcSubject AS CHARACTER   NO-UNDO.
  DEFINE BUFFER bf-po-ord FOR po-ord.
  DEFINE VARIABLE llAttachExists AS LOG NO-UNDO.
  SESSION:SET-WAIT-STATE ("").
  llAttachExists = NO.

  IF v-print-fmt <> "southpak-xl" THEN
  DO WITH FRAME {&FRAME-NAME}:  
    CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
       WHEN 4 THEN DO:

           IF lv-fax-type = "MULTI" THEN DO:
              RUN output-to-fax-prt. /* create tif file */              
              {custom/asifaxm3.i &TYPE         = "MULTI"
                                &begin_cust   = ip-begin-vend-no
                                &end_cust     = ip-end-vend-no
                                &fax-subject  = "Purchase Orders"
                                &fax-body     = "Purchase Orders"
                                &fax-file     = lv-fax-image
                                &end-widget   = end_vend-no }      
           END.
           ELSE DO:
           {custom/asifax.i     &type         = "Vendor"
                                &begin_cust   = begin_vend-no
                                &END_cust     = begin_vend-no
                                &fax-subject  = "Purchase Orders"
                                &fax-body     = "Purchase Orders"
                                &fax-file     = list-name}
           END.
       END. 

       WHEN 6 THEN RUN output-to-port.
    END CASE. 
  END.

  IF rd-dest = 5 THEN DO:      

    /* gdm - 11190804 */
    IF LOOKUP(v-print-fmt,"Xprint,poprint 1,poprint 10,LancoYork,StClair,Boss,Hughes,PeachTree,FibreX,Lovepac,POPrint10-CAN,Protagon") > 0 
        OR lv-attachments THEN DO:
      FIND FIRST sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ cocode
          AND sys-ctrl.name    EQ "POPaperClip" 
          AND sys-ctrl.log-fld EQ YES NO-ERROR.
      IF AVAILABLE sys-ctrl OR lv-attachments THEN DO:        

        FOR EACH bf-attach NO-LOCK
            WHERE bf-attach.company EQ cocode
              AND TRIM(bf-attach.est-no) GE TRIM(STRING(v-start-po))
              AND TRIM(bf-attach.est-no) LE TRIM(STRING(v-end-po))
              AND bf-attach.attach-file NE "" .        
          IF bf-attach.est-no GT "" THEN DO:
            FIND FIRST bf-po-ord WHERE TRIM(STRING(bf-po-ord.po-no)) EQ bf-attach.est-no
              NO-LOCK NO-ERROR.
            IF NOT AVAILABLE bf-po-ord THEN
              NEXT.
            IF bf-attach.rec_key NE bf-po-ord.rec_key THEN
              NEXT.
          END.
          llattachExists = TRUE.
        END.
        IF llAttachExists THEN DO:

            IF AVAILABLE sys-ctrl AND sys-ctrl.int-fld EQ 1 THEN DO:
                v-outfile = "".
                FOR EACH bf-attach NO-LOCK
                    WHERE bf-attach.company EQ cocode
                      AND TRIM(bf-attach.est-no) GE TRIM(STRING(v-start-po))
                      AND TRIM(bf-attach.est-no) LE TRIM(STRING(v-end-po)):
                    IF bf-attach.est-no GT "" THEN DO:
                      FIND FIRST bf-po-ord WHERE TRIM(STRING(bf-po-ord.po-no)) EQ bf-attach.est-no
                        NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE bf-po-ord THEN
                          NEXT.
                        IF bf-attach.rec_key NE bf-po-ord.rec_key THEN
                          NEXT.
                    END.
                    ASSIGN 
                        v-outfile = v-outfile + TRIM(bf-attach.attach-file) + "," .

                END.
            END.
            ELSE
                RUN porep/d-poattd.w (INPUT  TRIM(STRING(cocode)),
                                      INPUT  TRIM(STRING(v-start-po)),
                                      INPUT  TRIM(STRING(v-end-po)),
                                      OUTPUT v-outfile).
        END.

      END.
    END.

    IF is-xprint-form OR v-print-fmt = "southpak-xl" THEN DO:

      IF v-print-fmt <> "southpak-xl" THEN DO:

        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").

        IF NOT AttachmentExists() THEN RETURN.
      END.

      ELSE 
        ASSIGN lv-pdf-file = init-dir + "\PO.pdf".

      /* gdm - 11190804 */
      IF (LOOKUP(v-print-fmt,"Xprint,poprint 1,poprint 10,LancoYork,StClair,Boss,Hughes,PeachTree,FibreX,Lovepac,POPrint10-CAN,Protagon") > 0 
           OR 
          lv-attachments)
        AND TRIM(v-outfile) NE "" 
         THEN ASSIGN lv-pdf-file = v-outfile + lv-pdf-file. 

      IF LOOKUP(v-print-fmt,"PremierX,PremierCX,PremierXFGItems,Centbox") > 0  THEN DO: 
          IF v-start-po EQ v-end-po  THEN
            lcSubject = "Purchase Order: " + STRING(v-start-po).
          ELSE
            lcSubject = "Purchase Orders: " + STRING(v-start-po) + " - " + STRING(v-end-po).
      END.
      ELSE lcSubject = "Purchase Orders".

      RUN custom/xpmail2.p   (INPUT   "Vendor",
                              INPUT   'R-POPRT.',
                              INPUT   lv-pdf-file,
                              INPUT   begin_vend-no,
                              INPUT   lcSubject,
                              INPUT   "Purchase Orders",
                              OUTPUT  vcErrorMsg).
    END.

    ELSE DO:  

      IF NOT AttachmentExists() THEN RETURN.

      /* gdm - 11190804 */
      IF (LOOKUP(v-print-fmt,"Xprint,poprint 1,poprint 10,LancoYork,StClair,Boss,Hughes,PeachTree,FibreX,Lovepac,POPrint10-CAN,Protagon") > 0 
           OR
          lv-attachments)
        AND TRIM(v-outfile) NE "" 
         THEN ASSIGN lv-pdf-file = v-outfile + lv-pdf-file. 

      RUN custom/xpmail2.p   (INPUT   "Vendor",
                              INPUT   'R-POPRT.',
                              INPUT   lv-pdf-file,
                              INPUT   begin_vend-no,
                              INPUT   "Purchase Orders",
                              INPUT   "Purchase Orders",
                              OUTPUT  vcErrorMsg).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE help-po-no C-Win 
PROCEDURE help-po-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-type AS INTEGER NO-UNDO.

  DEFINE VARIABLE char-val AS cha NO-UNDO.
  DEFINE VARIABLE lv-po-no AS CHARACTER NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    lv-po-no = IF ip-type EQ 1 THEN begin_po-no:SCREEN-VALUE
                               ELSE end_po-no:SCREEN-VALUE.

    RUN windows/l-poopen.w (cocode, lv-po-no, OUTPUT char-val).
    IF char-val NE "" THEN lv-po-no = ENTRY(1,char-val).

    IF ip-type EQ 1 THEN DO:
      begin_po-no:SCREEN-VALUE = lv-po-no.
      APPLY "entry" TO begin_po-no.
    END.
    ELSE DO:
      end_po-no:SCREEN-VALUE   = lv-po-no.
      APPLY "entry" TO end_po-no.
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
     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-fax-prt C-Win 
PROCEDURE output-to-fax-prt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VARIABLE lv-file-name AS cha FORM "x(60)" NO-UNDO.
  DEFINE VARIABLE lv-xpr-file AS cha FORM "x(60)" NO-UNDO.

  IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     INPUT FROM OS-DIR ("C:\temp\fax") NO-ECHO.
     REPEAT:
        SET lv-file-name.
        IF lv-file-name <> "." AND lv-file-name <> ".."  AND lv-file-name MATCHES "*xpr*" 
        THEN DO:
             lv-xpr-file = "c:\temp\fax\" + lv-file-name.             
             RUN printfile (lv-xpr-file).
        END.
        lv-file-name = "".   
     END.

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
     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

     IF init-dir = "" THEN init-dir = "c:\temp" .
     SYSTEM-DIALOG GET-FILE list-name
         TITLE      "Enter Listing Name to SAVE AS ..."
         FILTERS    "Listing Files (*.rpt)" "*.rpt",
                    "All Files (*.*)" "*.*"
         INITIAL-DIR init-dir
         ASK-OVERWRITE
         SAVE-AS
         USE-FILENAME

         UPDATE OKpressed.

     IF NOT OKpressed THEN  RETURN NO-APPLY.


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
  ELSE RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* --------------------------------------------------- po/po-print.p 10/94 rd */
/* Purchase Order Print Program - P/O Module                                  */
/* -------------------------------------------------------------------------- */

  DEFINE INPUT PARAMETER icVendNo AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

  {sys/form/r-top.i}

  ASSIGN
    v-start-po          = begin_po-no
    v-end-po            = end_po-no
    v-reprint-po        = tb_reprint
    v-printde-po        = tb_delete
    v-print-sn          = tb_spec
    v-corrugator        = tb_corr
    v-sendfax           = NO
    v-faxprog           = ""
    v-tmp-fax           = ""
    s-group-notes       = tb_group-notes
    v-summarize-by-item = tb_summarize-by-item
    v-itemDescription   = tb_itemDescription
    v-score-types       = tb_score-types
    v-metric            = tb_metric
    s-print-prices      = tb_print-prices
    v-print-terms       = tb_print-terms
    lv-attachments      = tb_attachments
    lCustCode           =  tb_cust-code.

  IF ip-sys-ctrl-shipto THEN
     ASSIGN
        v-start-vend = icVendNo
        v-end-vend   = icVendNo.
  ELSE
     ASSIGN
        v-start-vend = begin_vend-no
        v-end-vend   = end_vend-no.

  IF rd-dest EQ 4 THEN DO:

    v-sendfax = YES.

    FIND FIRST sys-ctrl
         WHERE sys-ctrl.company EQ cocode
           AND sys-ctrl.name    EQ "FAXSOFT"  NO-LOCK NO-ERROR.

    IF NOT AVAILABLE sys-ctrl THEN DO TRANSACTION:

      CREATE sys-ctrl.

      ASSIGN
       sys-ctrl.company  = cocode
       sys-ctrl.name     = "FAXSOFT"
       sys-ctrl.descrip  = "Fax Software"
       sys-ctrl.char-fld = "FaxConsl"
       sys-ctrl.log-fld  = NO.

      MESSAGE "System control record NOT found. " "Enter Fax Software"
              UPDATE sys-ctrl.char-fld.
    END.

    v-faxprog = sys-ctrl.char-fld. 
  END.

  {sa/sa-sls01.i}

  v-term-id = v-term.

  FOR EACH po-ord NO-LOCK
     WHERE po-ord.company EQ cocode
       AND po-ord.printed EQ v-reprint-po
       AND po-ord.po-no   GE v-start-po
       AND po-ord.po-no   LE v-end-po
       AND po-ord.vend-no GE v-start-vend
       AND po-ord.vend-no LE v-end-vend:

    IF NOT(po-ord.stat EQ "N" OR po-ord.stat EQ "O" OR po-ord.stat EQ "U" OR
       (tb_reprint-closed AND po-ord.stat EQ "C")) THEN
       NEXT.

    CREATE report.
    ASSIGN
     report.term-id = v-term
     report.key-01  = po-ord.vend-no
     report.key-02  = STRING(po-ord.po-no,"9999999999")
     report.rec-id  = RECID(po-ord).
  END.

  {sys/inc/print1.i}

  {sys/inc/outprint.i VALUE(lines-per-page)}

  IF td-show-parm THEN RUN show-param.

  v-lines-per-page = lines-per-page.

  IF IS-xprint-form AND v-print-fmt <> "southpak-xl" THEN DO:

      CASE rd-dest:
          WHEN 1 THEN PUT  "<PRINTER?></PROGRESS>".
          WHEN 2 THEN do:
              IF NOT lBussFormModle THEN
                PUT "<PREVIEW><MODAL=NO></PROGRESS>".     
              ELSE
                PUT "<PREVIEW></PROGRESS>".     
          END.          
          WHEN 4 THEN DO:
              ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
                    /*(IF is-xprint-form THEN ".xpr" ELSE ".txt").*/
              PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW></PROGRESS>".
          END.
          WHEN 5 THEN DO:
              IF v-print-fmt = "Centbox" OR v-print-fmt = "VALLEY" 
                THEN PUT "<PREVIEW><FORMAT=LETTER></PROGRESS><PDF-EXCLUDE=MS Mincho><PDF-LEFT=2mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
                ELSE PUT "<PREVIEW><FORMAT=LETTER></PROGRESS><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file  + ".pdf>" FORM "x(180)".
          END.

      END CASE.
  END.

  IF LOOKUP(v-print-fmt,"SOUTHPAK,SouthPak-xl,CENTBOX,Oracle,metro,ASIXprnt,Valley,CSC-GA,HPB,Indiana,XPRINT,poprint 1,poprint 10,LancoYork,StClair,Boss,Hughes,PeachTree,ACPI,Sultana,CCC,SouleMed,Soule") > 0 THEN 
    RUN VALUE(v-program) (lv-multi-faxout,lines-per-page). 
  ELSE  
    RUN VALUE(v-program).

  FOR EACH reftable WHERE reftable.reftable EQ "vend.poexport" TRANSACTION:
    FIND FIRST vend
         WHERE vend.company   EQ reftable.company
           AND vend.vend-no   EQ reftable.code
           AND vend.po-export EQ "" NO-ERROR.
    IF AVAILABLE vend THEN vend.po-export = reftable.dscr.
    DELETE reftable.
  END.

  IF v-corrugator AND poexport-log THEN DO:
    FOR EACH w-export:
      DELETE w-export.
    END.

    IF poexport-cha EQ "Vendor" THEN
    FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
       FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id NO-LOCK,
       FIRST vend NO-LOCK
       WHERE vend.company   EQ po-ord.company
         AND vend.vend-no   EQ po-ord.vend-no
         AND vend.po-export NE ""
       BREAK 
          BY vend.po-export:

      IF FIRST-OF(vend.po-export) THEN RUN create-export (vend.po-export).
    END.

    ELSE RUN create-export (poexport-cha).

    FOR EACH w-export WHERE TRIM(w-exp-prog) NE "":

        RUN VALUE("po/" + TRIM(w-exp-prog) + ".p") (v-print-fmt) NO-ERROR.
    END.
  END.

  FOR EACH report WHERE report.term-id EQ v-term-id: 
    DELETE report.
  END.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetGlobalVariables C-Win 
PROCEDURE SetGlobalVariables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-po-ord-no AS INTEGER NO-UNDO.

  IF LOOKUP(v-print-fmt,"Pacific,Xprint,poprint 1,poprint 10,LancoYork,StClair,Boss,PeachTree,Xprint2,poprint 2,poprint 20,Southpak,Hughes,CENTbox,Oracle,metro,PremierX,PremierCX,PremierXFGItems,Protagon,Protagon2,Coburn,CSC,Elite,ottpkg,APC,consbox,FibreX,Lovepac,POPrint10-CAN,ASIXprnt,Valley,PPI,CSC-GA,HPB,Indiana,MWFibre,Packrite,Allwest,Bell,ACPI,Sultana,Badger,CCC,SouleMed,Soule") > 0 
    THEN is-xprint-form = YES.
    ELSE is-xprint-form = NO.

  ASSIGN
     lv-multi-faxout = IF rd-dest EQ 4 AND (begin_vend-no NE end_vend-no  OR
                                            is-xprint-form) THEN
                          YES
                       ELSE
                          NO
     lv-fax-type = IF lv-multi-faxout THEN "MULTI" 
                                      ELSE "VENDOR"
     lv-pdf-file = init-dir + (IF v-print-fmt EQ "Centbox" THEN
                                  "\CBXPO" 
                               ELSE
                                  "\PO") + string(ip-po-ord-no) NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetPOPrintForm C-Win 
PROCEDURE SetPOPrintForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER icPrintFormat  AS CHARACTER NO-UNDO.

  v-print-fmt = icPrintFormat.

  CASE icPrintFormat:

    WHEN 'MiddleSx'     THEN ASSIGN v-program = "po/pomidsex.p"     li-lineperpage = 64.
    WHEN 'Century'      THEN ASSIGN v-program = "po/po-cent.p"      li-lineperpage = 56. /* was 63*/    
    WHEN 'Rudd'         THEN ASSIGN v-program = "po/po-rudd.p"      li-lineperpage = 60.
    WHEN 'Brick'        THEN ASSIGN v-program = "po/po-brick.p"     li-lineperpage = 60.
    WHEN 'Fibre'        THEN ASSIGN v-program = "po/po-fibre.p"     li-lineperpage = 56.
    WHEN 'Pacific'      THEN ASSIGN v-program = "po/po-pacif.p"     li-lineperpage = 80.
    WHEN 'Elite'        THEN ASSIGN v-program = "po/po-elite.p"     li-lineperpage = 80.
    WHEN 'CSC'          THEN ASSIGN v-program = "po/po-xcsc.p"      li-lineperpage = 80.
    WHEN 'Xprint' OR    WHEN 'poprint 1'     THEN ASSIGN v-program = "po/po-xprnt.p"     li-lineperpage = 80.
    WHEN 'LancoYork'    THEN ASSIGN v-program = "po/po-lanyork.p"     li-lineperpage = 80.
    WHEN 'Lovepac'      THEN ASSIGN v-program = "po/po-loveten.p"     li-lineperpage = 60.
    WHEN 'POPrint10-CAN' THEN ASSIGN v-program = "po/po-lovetencan.p"     li-lineperpage = 60.
    WHEN 'poprint 10'   THEN ASSIGN v-program = "po/po-xprnt10.p"     li-lineperpage = 80.
    WHEN 'StClair'      THEN ASSIGN v-program = "po/po-stclr.p"     li-lineperpage = 80.
    WHEN 'Boss'         THEN ASSIGN v-program = "po/po-boss.p"     li-lineperpage = 80.
    WHEN 'PPI'          THEN ASSIGN v-program = "po/po-ppi.p"       li-lineperpage = 80.
    WHEN 'FibreX'       THEN ASSIGN v-program = "po/po-fibx.p"      li-lineperpage = 60.
    WHEN 'ConsBox'      THEN ASSIGN v-program = "po/po-consb.p"     li-lineperpage = 80.
    WHEN 'APC'          THEN ASSIGN v-program = "po/po-consb.p"     li-lineperpage = 80.
    WHEN 'Xprint2' OR WHEN 'poprint 2'     THEN ASSIGN v-program = "po/po-xprt2.p"     li-lineperpage = 80.
    WHEN 'poprint 20'     THEN ASSIGN v-program = "po/po-xprt20.p"     li-lineperpage = 80.
    WHEN 'OTTpkg'       THEN ASSIGN v-program = "po/po-ott.p"       li-lineperpage = 80.
    WHEN 'Hughes'       THEN ASSIGN v-program = "po/po-hughs.p"     li-lineperpage = 80.
    WHEN 'southpak'     THEN ASSIGN v-program = "po/po-sthpk.p"     li-lineperpage = 80.
    WHEN 'Indiana'      THEN ASSIGN v-program = "po/po-indiana.p"   li-lineperpage = 80.
    WHEN 'CSC-GA'       THEN ASSIGN v-program = "po/po-cscga.p"     li-lineperpage = 80.
    WHEN "southpak-xl"  THEN ASSIGN v-program = "po/po-sthpk-xl.p"  li-lineperpage = 80.  
    WHEN "asixprnt"     THEN ASSIGN v-program = "po/po-asix.p"      li-lineperpage = 80.  
    WHEN "PremierX"     THEN ASSIGN v-program = "po/po-xprem.p"     li-lineperpage = 80.  
    WHEN "PremierXFGItems"     THEN ASSIGN v-program = "po/po-xpremfg.p"     li-lineperpage = 80.  
    WHEN "PremierCX"    THEN ASSIGN v-program = "po/po-cxprem.p"     li-lineperpage = 80.
    WHEN "Protagon"     THEN ASSIGN v-program = "po/po-protg.p"     li-lineperpage = 85.  
    WHEN "Protagon2"    THEN ASSIGN v-program = "po/po-protg2.p"    li-lineperpage = 85.
    WHEN "Coburn"       THEN ASSIGN v-program = "po/po-coburn.p"    li-lineperpage = 85.
    WHEN "Centbox"      THEN ASSIGN v-program = "po/po-centx.p"     li-lineperpage = 80.  
    WHEN "Valley"       THEN ASSIGN v-program = "po/po-valy.p"      li-lineperpage = 80.  
    WHEN "Oracle"       THEN ASSIGN v-program = "po/po-oracl.p"     li-lineperpage = 80.
    WHEN "HPB"          THEN ASSIGN v-program = "po/po-hpb.p"       li-lineperpage = 80. 
    WHEN "metro"        THEN ASSIGN v-program = "po/po-metro.p"     li-lineperpage = 80.  
    WHEN "MWFibre"      THEN ASSIGN v-program = "po/po-mwfiber.p"   li-lineperpage = 80.
    WHEN 'Packrite'     THEN ASSIGN v-program = "po/po-pkrit.p"     li-lineperpage = 80.
    WHEN 'Allwest'      THEN ASSIGN v-program = "po/po-allws.p"     li-lineperpage = 80.
    WHEN 'Bell'         THEN ASSIGN v-program = "po/po-bell.p"      li-lineperpage = 80.
    WHEN 'ACPI'         THEN ASSIGN v-program = "po/po-acpi.p"      li-lineperpage = 80.                                                   
    WHEN 'Sultana'         THEN ASSIGN v-program = "po/po-sultn.p"      li-lineperpage = 80.                                                   
    WHEN 'Badger'       THEN ASSIGN v-program = "po/po-badgr.p"     li-lineperpage = 80.
    WHEN 'CCC'          THEN ASSIGN v-program = "po/po-ccc.p"       li-lineperpage = 80.
    WHEN 'Soule'        THEN ASSIGN v-program = "po/po-soule.p"     li-lineperpage = 80.
    WHEN 'SouleMed'     THEN ASSIGN v-program = "po/po-soulemed.p"  li-lineperpage = 80.
    WHEN 'PeachTree'    THEN ASSIGN v-program = "po/po-pchtree.p"   li-lineperpage = 80.
    OTHERWISE                
       ASSIGN v-program       = "po/po-asi.p"       
              li-lineperpage  =  IF v-print-fmt EQ "Sonoco" THEN 66 ELSE 60.
  END CASE.

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
  DEFINE VARIABLE lv-frame-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-group-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-field-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-field2-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE parm-fld-list AS cha NO-UNDO.
  DEFINE VARIABLE parm-lbl-list AS cha NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-label AS cha.

  lv-frame-hdl = FRAME {&FRAME-NAME}:HANDLE.
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
  lv-field-hdl = lv-group-hdl:FIRST-CHILD .

  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
        THEN DO:
           IF lv-field-hdl:LABEL <> ? THEN
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                     .
           ELSE DO:  /* radio set */
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     .
              lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
              REPEAT:
                  IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE.
                  IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN DO:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
                  END.
                  lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.
              END.       
           END.                 
     END.
     lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
  END.

  PUT SPACE(28)
      "< Selection Parameters >"
      SKIP(1).

  DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
    IF ENTRY(i,parm-fld-list) NE "" OR
       ENTRY(i,parm-lbl-list) NE "" THEN DO:

      lv-label = FILL(" ",34 - LENGTH(TRIM(ENTRY(i,parm-lbl-list)))) +
                 TRIM(ENTRY(i,parm-lbl-list)) + ":".

      PUT lv-label FORMAT "x(35)" AT 5
          SPACE(1)
          TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
          SKIP.              
    END.
  END.

  PUT FILL("-",80) FORMAT "x(80)" SKIP.
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AttachmentExists C-Win 
FUNCTION AttachmentExists RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF SEARCH (lv-pdf-file) EQ ? THEN DO:

      IF SEARCH (lv-pdf-file + '.pdf') EQ ? THEN DO:

        IF SEARCH (list-name) NE ? THEN DO:

          OS-RENAME VALUE (list-name) VALUE (list-name + '.txt').

          IF OS-ERROR NE 0 THEN DO:

            MESSAGE 'Failed to rename attachment file.' SKIP
                    'OS-ERROR: ' OS-ERROR
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

            RETURN FALSE.
          END.

          ELSE lv-pdf-file = list-name + '.txt'.
        END.
      END.

      ELSE
        lv-pdf-file = lv-pdf-file + '.pdf'.

      IF SEARCH (lv-pdf-file) = ? THEN DO:
        MESSAGE 'Attachment File: ' lv-pdf-file ' is missing.'
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN FALSE.
      END.
    END.

    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

