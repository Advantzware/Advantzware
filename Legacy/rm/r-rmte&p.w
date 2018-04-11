&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 

/*------------------------------------------------------------------------

  File: r-rmte&p.w

  Description: RM Edit List & Posting

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

/* Parameters Definitions ---                                           */
/*&IF DEFINED(UIB_IS_RUNNING) NE 0 &THEN
DEF VAR ip-post AS LOG INIT NO NO-UNDO.
&ELSE
DEF INPUT PARAMETER ip-post AS LOG NO-UNDO.
&ENDIF*/

DEF VAR ip-post AS LOG NO-UNDO.

IF INDEX(PROGRAM-NAME(1),"rm/r-rmtpst") NE 0 OR
   INDEX(PROGRAM-NAME(1),"rm/r-rmte&p") NE 0 THEN
   ip-post = YES.

/* Local Variable Definitions ---                                       */
DEF VAR list-name AS cha NO-UNDO.
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

DEF NEW SHARED VAR pox LIKE po-ordl.po-no.

DEF VAR v-types AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-autoissue AS LOG.
DEF VAR v-dunne AS LOG INIT NO.
DEF VAR v-rmtags-log AS LOG NO-UNDO.
DEF VAR iPo AS INT NO-UNDO.
DEF VAR lFromSS AS LOG NO-UNDO.
DEFINE VARIABLE dFirstDate AS DATE        NO-UNDO INIT ?.
DEFINE VARIABLE dLastDate  AS DATE        NO-UNDO INIT ?.
DEFINE VARIABLE cFirstJob  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLastJob   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iFirstJob2 AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLastJob2  AS INTEGER     NO-UNDO.
DEFINE VARIABLE lBadIssuesPrompted AS LOGICAL     NO-UNDO.
DEF TEMP-TABLE tt-bad-issues
  FIELD issue-rowid AS ROWID.
DEF VAR v-uid-sec AS LOG NO-UNDO.
DEF VAR v-access-close AS LOG NO-UNDO.
DEF VAR v-access-list AS CHAR NO-UNDO.
DEF VAR v-source-handle AS HANDLE NO-UNDO.
DEFINE VARIABLE cRtnChar          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE RmKeepZeroBin-log AS LOGICAL   NO-UNDO.
{jc/jcgl-sh.i NEW}

DEF TEMP-TABLE tt-rctd NO-UNDO LIKE rm-rctd FIELD tt-row-id AS ROWID
                                    FIELD rm-row-id AS ROWID
                                    FIELD has-rec   AS LOG INIT NO
                                    FIELD seq-no    AS INT
                                    FIELD db-seq-no AS INT 
                                    INDEX seq-no seq-no i-no.

DEF TEMP-TABLE tt-mat NO-UNDO FIELD frm LIKE job-mat.frm
                              FIELD qty LIKE job-mat.qty
                              INDEX frm frm.

DEF TEMP-TABLE tt-email NO-UNDO
    FIELD vend-no      AS CHAR 
    FIELD po-no        AS INT
    FIELD item-no      AS CHAR
    FIELD item-name    AS CHAR
    FIELD po-qty       AS DEC
    FIELD recvd-qty    AS DEC
    FIELD total-recvd-qty AS DEC
    FIELD cons-uom        AS CHAR
    FIELD overrun-pct AS DEC
    FIELD underrun-pct AS DEC
    FIELD allow-qty    AS DEC  
    FIELD under-qty    AS DEC
    FIELD undovr       AS CHAR
    INDEX po-no po-no ASC item-no ASC.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "AUTOISSU"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "AUTOISSU"
   sys-ctrl.descrip = "Automatically Issue RM Receipts to Jobs?".
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
END.
v-autoissue = sys-ctrl.log-fld.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name EQ "RMTAGS"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN
   DO TRANSACTION:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company = cocode
         sys-ctrl.name    = "RMTAGS"
         sys-ctrl.descrip = "Number of RM Loadtags to Print & Create Wip Tags"
         sys-ctrl.char-fld = ""
         sys-ctrl.int-fld = 1
         sys-ctrl.log-fld = FALSE. /* true create wip/false do not */
   END.

ASSIGN v-rmtags-log = sys-ctrl.log-fld.

DO TRANSACTION:
  {sys/inc/rmpostgl.i}
END.

  /* Check if authorized to create PO's */
RUN methods/prgsecur.p
    (INPUT "RMPostUID",
     INPUT "ALL", /* based on run, create, update, delete or all */
     INPUT NO,    /* use the directory in addition to the program */
     INPUT NO,    /* Show a message if not authorized */
     INPUT NO,    /* Group overrides user security? */
     OUTPUT v-uid-sec, /* Allowed? Yes/NO */
     OUTPUT v-access-close, /* used in template/windows.i  */
     OUTPUT v-access-list). /* list 1's and 0's indicating yes or no to run, create, update, delete */
     
RUN sys/ref/nk1look.p (INPUT cocode, "RMKEEPZEROBIN", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    RmKeepZeroBin-log = LOGICAL(cRtnChar) NO-ERROR.
    
DEF VAR v-pr-tots AS LOG FORMAT "Y/N" NO-UNDO.
DEF {1} SHARED VAR v-print-fmt  AS CHAR NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO. 

DEF BUFFER b-job-hdr FOR job-hdr.
DEF BUFFER b-rh FOR rm-rcpth.
DEF BUFFER b-rd FOR rm-rdtlh.

/* AJ 06/26/2008  Added two variables for excel report */
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
&Scoped-Define ENABLED-OBJECTS RECT-18 RECT-6 fiAutoIssue v-post-date ~
v-from-job begin_job-no2 v-to-job end_job-no2 ldt-from ldt-to begin_userid ~
end_userid t-receipt t-issue t-trans t-adj t-showtotal lv-ornt ~
lines-per-page rd-dest lv-font-no td-show-parm tb_excel tb_runExcel fi_file ~
Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fiAutoIssue v-post-date v-from-job ~
begin_job-no2 v-to-job end_job-no2 ldt-from ldt-to begin_userid end_userid ~
t-receipt t-issue t-trans t-adj t-showtotal lv-ornt lines-per-page rd-dest ~
lv-font-no td-show-parm lv-font-name tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD is-run-from-ss C-Win 
FUNCTION is-run-from-ss RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE VARIABLE begin_job-no2 AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4.4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_userid AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning User ID" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS INTEGER FORMAT ">9":U INITIAL 99 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4.6 BY 1 NO-UNDO.

DEFINE VARIABLE end_userid AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending User ID" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiAutoIssue AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     LABEL "Create Issues?" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-mchshf.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE ldt-from AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE ldt-to AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

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

DEFINE VARIABLE tran-period AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE v-from-job AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE v-post-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Post Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE v-to-job AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

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

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 12.14.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 8.81.

DEFINE VARIABLE t-adj AS LOGICAL INITIAL no 
     LABEL "Adjustments" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE t-issue AS LOGICAL INITIAL no 
     LABEL "Issues" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE t-receipt AS LOGICAL INITIAL no 
     LABEL "Receipts" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .86 NO-UNDO.

DEFINE VARIABLE t-showtotal AS LOGICAL INITIAL no 
     LABEL "Show Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE t-trans AS LOGICAL INITIAL no 
     LABEL "Transfers" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

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
     fiAutoIssue AT ROW 1.48 COL 74 COLON-ALIGNED WIDGET-ID 2
     v-post-date AT ROW 3.14 COL 20 COLON-ALIGNED
     tran-period AT ROW 3.14 COL 63 COLON-ALIGNED
     v-from-job AT ROW 4.33 COL 20 COLON-ALIGNED
     begin_job-no2 AT ROW 4.33 COL 37 COLON-ALIGNED
     v-to-job AT ROW 4.33 COL 63 COLON-ALIGNED
     end_job-no2 AT ROW 4.33 COL 80 COLON-ALIGNED
     ldt-from AT ROW 5.52 COL 20 COLON-ALIGNED HELP
          "Enter the Beginning Date"
     ldt-to AT ROW 5.52 COL 63 COLON-ALIGNED HELP
          "Enter the Ending Date"
     begin_userid AT ROW 6.62 COL 20 COLON-ALIGNED HELP
          "Enter the Beginning User ID"
     end_userid AT ROW 6.62 COL 63 COLON-ALIGNED HELP
          "Enter the Ending User ID"
     t-receipt AT ROW 7.76 COL 44
     t-issue AT ROW 8.71 COL 44
     t-trans AT ROW 9.67 COL 44
     t-adj AT ROW 10.62 COL 44
     t-showtotal AT ROW 12.05 COL 44
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
     Btn_OK AT ROW 22 COL 22
     Btn_Cancel AT ROW 22.05 COL 59
     "Selection Parameters" VIEW-AS TEXT
          SIZE 22 BY .95 AT ROW 1.48 COL 6
          BGCOLOR 2 
     "Transaction Types" VIEW-AS TEXT
          SIZE 21 BY .86 AT ROW 7.76 COL 23
     "Output Destination" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 13.38 COL 2
     RECT-18 AT ROW 1 COL 1
     RECT-6 AT ROW 13.14 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 92.8 BY 23.1.


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
         TITLE              = "Raw Material Post"
         HEIGHT             = 23.1
         WIDTH              = 92.8
         MAX-HEIGHT         = 48.05
         MAX-WIDTH          = 322.8
         VIRTUAL-HEIGHT     = 48.05
         VIRTUAL-WIDTH      = 322.8
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
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

ASSIGN 
       Btn_Cancel:PRIVATE-DATA IN FRAME FRAME-F     = 
                "ribbon-button".

ASSIGN 
       Btn_OK:PRIVATE-DATA IN FRAME FRAME-F     = 
                "ribbon-button".

ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-F
   NO-ENABLE                                                            */
ASSIGN 
       t-adj:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

ASSIGN 
       t-issue:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

ASSIGN 
       t-receipt:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

ASSIGN 
       t-trans:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-F
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-F
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tran-period:HIDDEN IN FRAME FRAME-F           = TRUE.

ASSIGN 
       v-from-job:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

ASSIGN 
       v-post-date:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

ASSIGN 
       v-to-job:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Raw Material Post */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Raw Material Post */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-F
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME FRAME-F /* Cancel */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME FRAME-F /* OK */
DO: 
  DEF VAR lv-post AS LOG NO-UNDO.
  DEF VAR lv-r-no LIKE rm-rctd.r-no NO-UNDO.
  DEF VAR lValidQty AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.    
  END.

  IF ip-post THEN DO:
     FIND FIRST period
         WHERE period.company EQ cocode
           AND period.pst     LE v-post-date
           AND period.pend    GE v-post-date
         NO-LOCK NO-ERROR.
     IF NOT AVAIL period THEN DO WITH FRAME {&FRAME-NAME}:
       MESSAGE "No period exists for this date..."
               VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO v-post-date.
       RETURN NO-APPLY.
     END.
  END.

  IF fiAutoIssue:SENSITIVE = TRUE AND
     fiAutoIssue:SCREEN-VALUE = "?" THEN DO:
      MESSAGE "Please select Auto Issue (yes or no)"
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fiAutoIssue.
      RETURN NO-APPLY.
  END.

  ASSIGN
   v-from-job = FILL(" ",6 - length(TRIM(INPUT v-from-job))) +
                trim(INPUT v-from-job) 
   v-to-job   = FILL(" ",6 - length(TRIM(INPUT v-to-job))) +
                trim(INPUT v-to-job) 
   v-types    = (IF t-receipt THEN "R" ELSE "") +
                (IF t-issue   THEN "I" ELSE "") +
                (IF t-trans   THEN "T" ELSE "") +
                (IF t-adj     THEN "A" ELSE "")
   v-pr-tots  = t-showtotal.

  FOR EACH work-gl:
    DELETE work-gl.
  END.

  RUN run-report (OUTPUT lValidQty). 
  IF NOT lValidQty THEN
    RETURN NO-APPLY.

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
       WHEN 4 THEN DO:
           /*run output-to-fax.*/
           {custom/asifax.i &type= ''
                            &begin_cust=v-from-job
                            &END_cust= v-to-job
                            &fax-subject=c-win:TITLE
                            &fax-body=c-win:TITLE
                            &fax-file=list-name }
       END. 
       WHEN 5 THEN DO:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = ''
                             &begin_cust= v-from-job
                             &END_cust=v-to-job
                             &mail-subject=c-win:TITLE
                             &mail-body=c-win:TITLE
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust= v-from-job
                                  &END_cust=v-to-job
                                  &mail-subject=c-win:TITLE
                                  &mail-body=c-win:TITLE
                                  &mail-file=list-name }

           END.
       END. 
      WHEN 6 THEN RUN output-to-port.
  END CASE.

  IF ip-post THEN DO: 
    lv-post = CAN-FIND(FIRST tt-rctd WHERE tt-rctd.has-rec).

    IF lv-post THEN
    FOR EACH tt-rctd
        WHERE tt-rctd.has-rec
          AND NOT CAN-FIND(FIRST rm-rctd WHERE ROWID(rm-rctd) EQ tt-rctd.rm-row-id):
      lv-post = NO.
      LEAVE.
    END.

    IF lv-post THEN DO:
      lv-post = NO.
      MESSAGE "Post RM Transactions?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE lv-post.
    END.

    ELSE MESSAGE "Sorry, nothing is available for posting..."
             VIEW-AS ALERT-BOX.

    IF lv-post THEN DO:
      FOR EACH tt-rctd
          WHERE tt-rctd.has-rec
            AND CAN-FIND(FIRST rm-rcpth WHERE rm-rcpth.r-no EQ tt-rctd.r-no),
          FIRST rm-rctd WHERE rm-rctd.r-no EQ tt-rctd.r-no:
        lv-r-no = rm-rctd.r-no.
        DO TRANSACTION:
          rm-rctd.r-no = 0.
        END.
        DO TRANSACTION:
          rm-rctd.r-no = lv-r-no.
        END.
        tt-rctd.r-no = rm-rctd.r-no.
      END.

      FOR EACH tt-rctd
          WHERE tt-rctd.has-rec
            AND NOT CAN-FIND(FIRST rm-rctd WHERE rm-rctd.r-no EQ tt-rctd.r-no),
          FIRST rm-rcpth NO-LOCK WHERE rm-rcpth.r-no EQ tt-rctd.r-no:
        MESSAGE "Sorry, these RM Transactions cannot be processed because 1 or " +
                "more have already been posted by UserID: " +
                TRIM(rm-rcpth.user-id) + "..."
            VIEW-AS ALERT-BOX ERROR.

        lv-post = NO.
        LEAVE.
      END.
    END.

    IF lv-post THEN DO:       
      RUN post-rm.

      lv-post = v-dunne.

      IF lv-post THEN MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.

      ELSE MESSAGE "Posting Incomplete..." VIEW-AS ALERT-BOX ERROR.
    END.
  END.

  RUN util/fxissues.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-F
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-F /* If Yes, File Name */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-F /* Lines Per Page */
DO:
  ASSIGN {&self-name}.
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
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-adj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-adj C-Win
ON VALUE-CHANGED OF t-adj IN FRAME FRAME-F /* Adjustments */
DO:
      ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-issue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-issue C-Win
ON VALUE-CHANGED OF t-issue IN FRAME FRAME-F /* Issues */
DO:
      ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-receipt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-receipt C-Win
ON VALUE-CHANGED OF t-receipt IN FRAME FRAME-F /* Receipts */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-trans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-trans C-Win
ON VALUE-CHANGED OF t-trans IN FRAME FRAME-F /* Transfers */
DO:
      ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-F /* Export To Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-F /* Auto Run Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-F /* Show Parameters? */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-from-job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-from-job C-Win
ON LEAVE OF v-from-job IN FRAME FRAME-F /* Beginning Job# */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-post-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-post-date C-Win
ON LEAVE OF v-post-date IN FRAME FRAME-F /* Post Date */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-to-job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-to-job C-Win
ON LEAVE OF v-to-job IN FRAME FRAME-F /* Ending Job# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


PROCEDURE mail EXTERNAL "xpMail.dll" :
      DEF INPUT PARAM mailTo AS CHAR.
      DEF INPUT PARAM mailsubject AS CHAR.
      DEF INPUT PARAM mailText AS CHAR.
      DEF INPUT PARAM mailFiles AS CHAR.
      DEF INPUT PARAM mailDialog AS LONG.
      DEF OUTPUT PARAM retCode AS LONG.
END.

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
/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

   DO TRANSACTION:  
      {sys/inc/rmemails.i}
      {sys/inc/postdate.i}
   END.

  ll-auto = IF v-autoissue = ? THEN NO ELSE v-autoissue.

  iLastJob2 = 99.
  iFirstJob2 = 0.
  lFromSS = DYNAMIC-FUNCTION('is-run-from-ss':U).
  FOR EACH rm-rctd
      WHERE rm-rctd.company   EQ cocode
        AND rm-rctd.rita-code NE "C"
        AND rm-rctd.user-id EQ  USERID("nosweat")
      NO-LOCK,
      FIRST item
      WHERE item.company EQ cocode
        AND item.i-no    EQ rm-rctd.i-no
      NO-LOCK
      BREAK BY rm-rctd.rita-code:

    IF dFirstDate = ? OR dFirstDate GT rm-rctd.rct-date THEN
      dFirstDate = rm-rctd.rct-date.
    IF dLastDate = ? OR dLastDate LT rm-rctd.rct-date THEN
      dLastDate = rm-rctd.rct-date.
    IF cFirstJob EQ "" OR cFirstJob GT rm-rctd.job-no THEN
      cFirstJob = rm-rctd.job-no.
    IF cLastJob EQ "" OR cLastJob LT rm-rctd.job-no THEN
      cLastJob = rm-rctd.job-no.
    IF iFirstJob2 EQ ? OR iFirstJob2 GT rm-rctd.Job-no2 THEN
      iFirstJob2 = rm-rctd.Job-no2.
    IF iLastJob2 EQ ? OR iLastJob2 LT rm-rctd.Job-no2 THEN
      iLastJob2 = rm-rctd.Job-no2.


    IF FIRST-OF(rm-rctd.rita-code) THEN v-types = v-types + rm-rctd.rita-code.

    IF NOT ll-auto AND rm-rctd.rita-code EQ "R" THEN
      ll-auto = (item.mat-type EQ "I" AND TRIM(rm-rctd.po-no) EQ "") OR
                (item.i-code EQ "E" AND TRIM(rm-rctd.po-no) NE "").
  END.

  IF ll-auto                 AND
     index(v-types,"R") GT 0 AND
     index(v-types,"I") EQ 0 THEN v-types = TRIM(v-types) + "I".

  ASSIGN
   v-post-date = TODAY
   t-receipt   = INDEX(v-types,"R") GT 0
   t-issue     = INDEX(v-types,"I") GT 0
   t-trans     = INDEX(v-types,"T") GT 0
   t-adj       = INDEX(v-types,"A") GT 0
   v-types     = ""
   c-win:TITLE = IF ip-post THEN "Raw Material Post"
                            ELSE "Raw Material Edit List".

  RUN enable_UI.

  IF NOT ip-post THEN
     v-post-date:HIDDEN = YES.
  ELSE
     RUN check-date.

  DO WITH FRAME {&FRAME-NAME}:
      {custom/usrprint.i}
    IF NOT lFromSS THEN DO:     /* task 10201406 */
    END.
    ELSE DO:
      ASSIGN
      ldt-from:SCREEN-VALUE = STRING(dFirstDate)
        ldt-to:SCREEN-VALUE = STRING(dLastDate)
        v-from-job:SCREEN-VALUE =  cFirstJob
        v-to-job:SCREEN-VALUE =  cLastJob
        begin_job-no2:SCREEN-VALUE =  string(iFirstJob2)
        end_job-no2:SCREEN-VALUE =  string(iLastJob2) .
    END.
    IF NOT ip-post THEN
       v-post-date:SCREEN-VALUE = STRING(TODAY).

    ASSIGN
     begin_userid:SCREEN-VALUE = USERID("nosweat")
     end_userid:SCREEN-VALUE   = USERID("nosweat").
    IF NOT v-uid-sec THEN
      DISABLE begin_userid END_userid.
    ASSIGN fiAutoIssue:SCREEN-VALUE = STRING(v-autoissue)
           fiAutoIssue:SENSITIVE = (IF v-autoissue = ? THEN YES ELSE NO).

/*     APPLY "entry" TO v-from-job. */

  END.

  {methods/nowait.i}

  IF v-post-date:HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
  DO:
     IF postdate-log THEN DO:
        v-post-date:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
        APPLY "ENTRY" TO v-from-job IN FRAME {&FRAME-NAME}.
     END.
     ELSE DO:
        v-post-date:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".    
        APPLY "entry" TO v-post-date IN FRAME {&FRAME-NAME}.
     END.
  END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-prep-info C-Win 
PROCEDURE assign-prep-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bf-tt-rctd FOR tt-rctd.
FOR EACH bf-tt-rctd   
    WHERE bf-tt-rctd.seq-no EQ tt-rctd.seq-no 
      AND bf-tt-rctd.i-no   EQ tt-rctd.i-no:
/*         BREAK BY bf-tt-rctd.seq-no   */
/*               BY bf-tt-rctd.i-no     */
/*               BY bf-tt-rctd.r-no     */
/*               BY bf-tt-rctd.job-no   */
/*               BY bf-tt-rctd.job-no2: */
/*   IF FIRST-OF(bf-tt-rctd.job-no) THEN DO:                                      */
/*       FOR EACH job-hdr NO-LOCK                                                 */
/*       WHERE job-hdr.company EQ bf-tt-rctd.company                              */
/*         AND job-hdr.job-no  EQ bf-tt-rctd.job-no                               */
/*         AND job-hdr.job-no2 EQ bf-tt-rctd.job-no2:                             */
/*       FIND FIRST itemfg                                                        */
/*         WHERE itemfg.company EQ job-hdr.company                                */
/*           AND itemfg.i-no    EQ job-hdr.i-no NO-LOCK NO-ERROR.                 */
/*       IF AVAIL itemfg THEN DO:                                                 */
/*                                                                                */
/*         IF itemfg.plate-no NE "" THEN DO:                                      */
/*           FIND FIRST prep                                                      */
/*              WHERE prep.company EQ cocode                                      */
/*                AND prep.code    EQ itemfg.plate-no NO-ERROR.                   */
/*            IF AVAIL prep THEN ASSIGN prep.received-date = bf-tt-rctd.rct-date. */
/*         END.                                                                   */
/*                                                                                */
/*         IF itemfg.die-no NE "" THEN DO:                                        */
/*           FIND FIRST prep                                                      */
/*             WHERE prep.company EQ cocode                                       */
/*               AND prep.code    EQ itemfg.die-no NO-ERROR.                      */
/*           IF AVAIL prep THEN ASSIGN prep.received-date = bf-tt-rctd.rct-date.  */
/*                                                                                */
/*         END.                                                                   */
/*         RELEASE prep.                                                          */
/*                                                                                */
/*       END. /* avail itemfg */                                                  */
/*     END. /* EACH job-hdr */                                                    */
/*     END. /*bf-tt-rctd.job ne ""*/                                              */
/*   END. /* FIRST-OF */                                                          */
/*Note (BPV) - code above was commented after discussing the simplicity of matching 
the RM item number between receipt record and prep record via the i-no
NOTE: this is not an indexed search but records in prep should not be a large # 
wfk - 9/6 - Changing to join on prep.code so that customers who use the same rm
in multiple prep codes are not affected. Also, should be indexed, so faster. */

    FOR EACH prep WHERE prep.company EQ cocode                          
             AND prep.CODE = bf-tt-rctd.i-no:
        ASSIGN
            prep.loc            = bf-tt-rctd.loc
            prep.loc-bin        = bf-tt-rctd.loc-bin
            prep.received-date  = bf-tt-rctd.rct-date.
        IF bf-tt-rctd.job-no NE "" THEN
            ASSIGN
                prep.last-job-no    = bf-tt-rctd.job-no
                prep.last-job-no2   = bf-tt-rctd.job-no2.
    END. /* each prep */
END. /* each tt-rctd   */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-date C-Win 
PROCEDURE check-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&frame-name}:
    ll-valid = YES.

    IF v-post-date:HIDDEN = NO THEN
    DO:
       FIND FIRST period                   
           WHERE period.company EQ cocode
             AND period.pst     LE v-post-date
             AND period.pend    GE v-post-date
           NO-LOCK NO-ERROR.
       IF AVAIL period THEN tran-period:SCREEN-VALUE = STRING(period.pnum).

       ELSE
       IF ip-post THEN DO:
         MESSAGE "No Defined Period Exists for" v-post-date VIEW-AS ALERT-BOX ERROR.
         ll-valid = NO.
       END.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkIssuedQty C-Win 
PROCEDURE checkIssuedQty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER oplValid AS LOGICAL     NO-UNDO.
DEF VAR iTotalI AS INT NO-UNDO.
DEF VAR iTotBin AS INT NO-UNDO.
ASSIGN
  iTotalI = 0
  iTotBin = 0.
  oplValid = YES.
  FOR EACH tt-rctd,
    FIRST rm-rctd WHERE ROWID(rm-rctd) EQ tt-rctd.rm-row-id
      AND rm-rctd.rita-code = "I"
     NO-LOCK
     BREAK BY rm-rctd.i-no
           BY rm-rctd.loc
           BY rm-rctd.loc-bin
           BY rm-rctd.tag:
    iTotalI = iTotalI + rm-rctd.qty.
    IF LAST-OF(rm-rctd.tag) THEN DO:
      FIND ITEM 
          WHERE ITEM.company EQ rm-rctd.company
            AND ITEM.i-no EQ rm-rctd.i-no
          NO-LOCK NO-ERROR.

      /* Only validate for stocked items */
      IF NOT ITEM.stocked THEN
        NEXT.

      FOR EACH rm-bin WHERE rm-bin.company EQ rm-rctd.company
        AND rm-bin.i-no EQ rm-rctd.i-no
        AND rm-bin.tag  EQ rm-rctd.tag
        AND rm-bin.loc  EQ rm-rctd.loc
        AND rm-bin.loc-bin EQ rm-rctd.loc-bin
        NO-LOCK.
        iTotBin = iTotBin + rm-bin.qty.
      END.

      IF iTotalI GT iTotBin THEN DO:
        IF NOT lBadIssuesPrompted THEN DO:

        MESSAGE "Quantity Issued is greater than Bins On Hand + Unposted Issues for item " + rm-rctd.i-no 
          + " and Tag " + rm-rctd.tag + "." SKIP
          "Please update Issues to post."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
          lBadIssuesPrompted = TRUE.
        END.
        oplValid = NO.
        CREATE tt-bad-issues.
        tt-bad-issues.issue-rowid = ROWID(rm-rctd).
      END.
      ASSIGN iTotalI = 0
             iTotBin = 0.
    END.
  END.

  FOR EACH tt-bad-issues:
      FIND FIRST tt-rctd WHERE tt-rctd.rm-row-id = tt-bad-issues.issue-rowid
        NO-ERROR.
      IF AVAIL tt-rctd THEN
        DELETE tt-rctd.
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
  DISPLAY fiAutoIssue v-post-date v-from-job begin_job-no2 v-to-job end_job-no2 
          ldt-from ldt-to begin_userid end_userid t-receipt t-issue t-trans 
          t-adj t-showtotal lv-ornt lines-per-page rd-dest lv-font-no 
          td-show-parm lv-font-name tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-F IN WINDOW C-Win.
  ENABLE RECT-18 RECT-6 fiAutoIssue v-post-date v-from-job begin_job-no2 
         v-to-job end_job-no2 ldt-from ldt-to begin_userid end_userid t-receipt 
         t-issue t-trans t-adj t-showtotal lv-ornt lines-per-page rd-dest 
         lv-font-no td-show-parm tb_excel tb_runExcel fi_file Btn_OK Btn_Cancel 
      WITH FRAME FRAME-F IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-F}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE final-steps C-Win 
PROCEDURE final-steps :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-tt-rctd FOR tt-rctd.
  DEF BUFFER rec-rm-rdtlh FOR rm-rdtlh.
  DEF BUFFER rec-rm-rcpth FOR rm-rcpth.

  DEF VAR v-int AS INT NO-UNDO.
  DEF VAR v-qty-received AS DEC NO-UNDO.

  IF rm-rctd.rita-code EQ "I" AND TRIM(rm-rctd.tag) NE "" THEN
     FOR EACH rec-rm-rdtlh NO-LOCK
         WHERE rec-rm-rdtlh.company   EQ rm-rctd.company
           AND rec-rm-rdtlh.tag       EQ rm-rctd.tag
           AND rec-rm-rdtlh.rita-code EQ "R"
         USE-INDEX tag,
         FIRST rec-rm-rcpth
         WHERE rec-rm-rcpth.r-no      EQ rec-rm-rdtlh.r-no
           AND rec-rm-rdtlh.rita-code EQ rec-rm-rdtlh.rita-code
         NO-LOCK:

       IF rm-rctd.po-no EQ "" THEN rm-rctd.po-no = rec-rm-rcpth.po-no.

       IF rm-rctd.job-no EQ "" THEN
         ASSIGN
          rm-rctd.job-no = rec-rm-rcpth.job-no
          rm-rctd.job-no2 = rec-rm-rcpth.job-no2.

       LEAVE.
     END.

    IF v-rmtags-log AND TRIM(rm-rctd.tag) NE "" THEN DO:
       FOR EACH wiptag WHERE wiptag.company = rm-rctd.company 
                         AND wiptag.rm-tag-no = rm-rctd.tag EXCLUSIVE-LOCK:
          ASSIGN
             wiptag.sts = "On Hand" .
       END.
    END.

  {rm/rm-rctd.i rm-rcpth rm-rdtlh rm-rctd} /* Create History Records */

  IF rm-rctd.rita-code EQ "R" THEN
  DO:
    {rm/rmemails.i}      
  END.

  DELETE rm-rctd.

  FOR EACH b-tt-rctd WHERE b-tt-rctd.tt-row-id EQ ROWID(tt-rctd):
    v-int = 0.
  RUN sys/ref/asiseq.p (INPUT g_company, INPUT "rm_rcpt_seq", OUTPUT v-int) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.


    CREATE rm-rctd.
    BUFFER-COPY b-tt-rctd TO rm-rctd
    ASSIGN
     rm-rctd.r-no        = v-int
     b-tt-rctd.r-no      = rm-rctd.r-no
     b-tt-rctd.has-rec   = YES
     b-tt-rctd.rm-row-id = ROWID(rm-rctd).    

  END.

  DELETE tt-rctd.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getRecordLock C-Win 
PROCEDURE getRecordLock :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    DEF INPUT PARAMETER iprRec AS RECID NO-UNDO.
    DEF INPUT PARAMETER ipcTable AS CHARACTER NO-UNDO.

    DEF VAR wrecid  AS INT NO-UNDO.
    DEF VAR wtable  AS INT NO-UNDO.
    DEF VAR iTryNum AS INT NO-UNDO.

    ASSIGN 
        wrecid = iprRec.
    FIND asi._file WHERE asi._file._file-name = ipcTable NO-LOCK.
    ASSIGN 
        wtable = asi._file._file-num.


    REPEAT:

        iTryNum = iTryNum + 1.

        &SCOP Filename cust
        &SCOP   lock        When "~{&FileName}" then do :           ~
                Find ~{&Filename} where RECID(~{&Filename}) = iprRec ~
                              EXCLUSIVE-LOCK NO-ERROR NO-WAIT.  ~
              IF avail(~{&Filename}) THEN RETURN. end.

        CASE ipcTable :
        &SCOP Filename item
            {&lock}
        &SCOP Filename rm-rctd
            {&lock}             
            OTHERWISE 
            RETURN ERROR.
        END CASE.



        IF iTryNum EQ 1 THEN 
        DO:
            PAUSE 1.
            NEXT.
        END.

        IF iTryNum EQ 2 THEN 
        DO:
            PAUSE 3.
            NEXT.
        END.


        /* iTryNum is 3, let the user know about it */

        /* Use repeat loop - More efficient than FIND ... WHERE
           due to lack of suitable index on _lock table */
        REPEAT:
            FIND NEXT asi._lock NO-LOCK NO-ERROR.
            IF NOT AVAIL asi._lock THEN 
                FIND FIRST asi._lock NO-LOCK NO-ERROR.
            IF _lock-recid = wrecid AND
                _lock-table = wtable AND
                (_lock-flag MATCHES "*X*" OR _lock-flag MATCHES "*S*")
                /* typically we're interested in any form of exclusive lock
                    so we test for X lock flag */
                THEN LEAVE.

            IF _lock-recid = ? THEN 
            DO:
                RELEASE asi._lock.
                LEAVE.
            END.
        END.

        IF AVAILABLE(asi._lock) THEN 
        DO:


            IF AVAIL asi._lock THEN
                FIND FIRST asi._connect WHERE _connect-usr = asi._lock._lock-usr NO-LOCK NO-ERROR.

            IF AVAIL asi._connect AND AVAIL asi._lock THEN
                MESSAGE "Waiting for " ipcTable " record to be unlocked." SKIP
                    "Currently locked by: " asi._lock._lock-name " user# " asi._lock._lock-usr SKIP
                    "ON " _connect-device SKIP
                    "Click OK to try again."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            ELSE
                MESSAGE "Waiting for " ipcTable " record to be unlocked." SKIP
                    "Click OK to try again."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

        END.
        iTryNum = 0.
        FIND FIRST asi._lock WHERE FALSE NO-LOCK NO-ERROR.
    END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gl-from-work C-Win 
PROCEDURE gl-from-work :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-run AS INT NO-UNDO.
  DEF INPUT PARAM ip-trnum AS INT NO-UNDO.

  DEF VAR credits AS DEC INIT 0 NO-UNDO.
  DEF VAR debits AS DEC INIT 0 NO-UNDO. 


  FIND FIRST period
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date
      NO-LOCK.

  FOR EACH work-gl 
      WHERE (ip-run EQ 1 AND work-gl.job-no NE "")
         OR (ip-run EQ 2 AND work-gl.job-no EQ "")
      BREAK BY work-gl.actnum:

    ASSIGN
     debits  = debits  + work-gl.debits
     credits = credits + work-gl.credits.

    IF LAST-OF(work-gl.actnum) THEN DO:
      CREATE gltrans.
      ASSIGN
       gltrans.company = cocode
       gltrans.actnum  = work-gl.actnum
       gltrans.jrnl    = "RMPOST"
       gltrans.period  = period.pnum
       gltrans.tr-amt  = debits - credits
       gltrans.tr-date = v-post-date
       gltrans.tr-dscr = IF work-gl.job-no NE "" THEN "RM Issue to Job"
                                                 ELSE "RM Receipt"
       gltrans.trnum   = ip-trnum
       debits  = 0
       credits = 0.

      RELEASE gltrans.
    END.
  END.

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
/*     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

     IF NOT OKpressed THEN  RETURN NO-APPLY.  */

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

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
  /*   RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
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
 RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-rm C-Win 
PROCEDURE post-rm :
/* --------------------------------------------------- rm/rm-post.p 10/94 rd  */
/* raw materials inventory control receipt maintenance                        */
/* -------------------------------------------------------------------------- */

DEF BUFFER xrm-rctd     FOR rm-rctd.
DEF BUFFER xrm-bin      FOR rm-bin.
DEF BUFFER b-rm-rctd    FOR rm-rctd.
DEF BUFFER b-item       FOR item.
DEF BUFFER b-po-ordl    FOR po-ordl.
DEF BUFFER b-job-mat    FOR job-mat.

DEF VAR v-avg-cst   AS LOG.
DEF VAR v-next_r-no LIKE rm-rctd.r-no.
DEF VAR v_r-no LIKE rm-rctd.r-no.
DEF VAR v-conv-qty AS DEC.
DEF VAR v-reduce-qty LIKE po-ordl.ord-qty.
DEF VAR no-of-items AS INT NO-UNDO.
DEF VAR ld-cvt-qty AS DEC NO-UNDO.
DEF VAR v-trnum LIKE gl-ctrl.trnum NO-UNDO.

DEF VAR v-r-qty     AS   DEC                    NO-UNDO.
DEF VAR v-i-qty     AS   DEC                    NO-UNDO.
DEF VAR v-t-qty     AS   DEC                    NO-UNDO.
DEF VAR cost        AS   DEC                    NO-UNDO.
DEF VAR out-qty     AS   DEC                    NO-UNDO.
DEF VAR v-bwt       LIKE item.basis-w           NO-UNDO.
DEF VAR v-len       LIKE item.s-len             NO-UNDO.
DEF VAR v-wid       LIKE item.s-wid             NO-UNDO.
DEF VAR v-dep       LIKE item.s-dep             NO-UNDO.
DEF VAR v-recid     AS   RECID                  NO-UNDO.
DEF VAR li          AS   INT                    NO-UNDO.

DEF VAR v-rmemail-file AS cha NO-UNDO.

FIND FIRST rm-ctrl WHERE rm-ctrl.company EQ cocode NO-LOCK NO-ERROR.
v-avg-cst = rm-ctrl.avg-lst-cst.

    SESSION:SET-WAIT-STATE ("general").
    transblok:
    FOR EACH tt-rctd
        WHERE CAN-FIND(FIRST item WHERE item.company EQ cocode
                                    AND item.i-no    EQ tt-rctd.i-no)
          AND INDEX(v-types,tt-rctd.rita-code) GT 0 
        BREAK BY tt-rctd.seq-no
              BY tt-rctd.i-no
              BY tt-rctd.r-no
              BY RECID(tt-rctd)

        TRANSACTION:

        RELEASE rm-rctd.
        RELEASE item.

      FIND rm-rctd NO-LOCK WHERE ROWID(rm-rctd) EQ tt-rctd.rm-row-id.
      RUN getRecordLock (RECID(rm-rctd), "rm-rctd").
      FIND rm-rctd EXCLUSIVE-LOCK WHERE ROWID(rm-rctd) EQ tt-rctd.rm-row-id.

      FIND FIRST item NO-LOCK
        WHERE item.company EQ rm-rctd.company
        AND item.i-no    EQ rm-rctd.i-no
        USE-INDEX i-no NO-WAIT NO-ERROR.

      RUN getRecordLock (RECID(item), "item").
      FIND FIRST item EXCLUSIVE-LOCK
          WHERE item.company EQ rm-rctd.company
            AND item.i-no    EQ rm-rctd.i-no
          USE-INDEX i-no .        

      IF rm-rctd.rita-code EQ "I" AND INT(rm-rctd.po-no) NE 0 THEN
      FOR EACH xrm-rctd
          WHERE xrm-rctd.company   EQ cocode
            AND xrm-rctd.i-no      EQ rm-rctd.i-no
            AND xrm-rctd.rita-code EQ "R"
            AND xrm-rctd.po-no     EQ rm-rctd.po-no
            AND xrm-rctd.r-no      LT rm-rctd.r-no
          NO-LOCK:

        UNDO transblok, NEXT transblok.
      END.

      FIND FIRST job
          WHERE job.company EQ rm-rctd.company
            AND job.job-no  EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no))) +
                               TRIM(rm-rctd.job-no)
            AND job.job-no2 EQ rm-rctd.job-no2
          NO-ERROR.

      /** Find Bin & if not avail then create it **/
      FIND FIRST rm-bin
          WHERE rm-bin.company EQ rm-rctd.company
            AND rm-bin.loc     EQ rm-rctd.loc
            AND rm-bin.i-no    EQ rm-rctd.i-no
            AND rm-bin.loc-bin EQ rm-rctd.loc-bin
            AND rm-bin.tag     EQ rm-rctd.tag
          NO-ERROR.
      IF NOT AVAIL rm-bin THEN DO:
        CREATE rm-bin.
        ASSIGN
         rm-bin.company = rm-rctd.company
         rm-bin.loc     = rm-rctd.loc
         rm-bin.loc-bin = rm-rctd.loc-bin
         rm-bin.tag     = rm-rctd.tag
         rm-bin.i-no    = rm-rctd.i-no.
      END. /* not avail rm-bin */

      ld-cvt-qty = rm-rctd.qty.

      iPo = INTEGER(rm-rctd.po-no) NO-ERROR.

      IF NOT ERROR-STATUS:ERROR AND NOT rm-rctd.rita-code EQ "T" AND NOT (rm-rctd.rita-code EQ "A" AND rm-rctd.qty LT 0) THEN
         rm-bin.po-no      = iPo.
      IF rm-rctd.pur-uom NE item.cons-uom AND item.cons-uom NE "" THEN
        RUN sys/ref/convquom.p (rm-rctd.pur-uom, item.cons-uom,
                              item.basis-w,
                              (IF item.r-wid EQ 0 THEN item.s-len ELSE 12), 
                              (IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid),
                              item.s-dep,
                              ld-cvt-qty, OUTPUT ld-cvt-qty).

      IF rm-rctd.rita-code EQ "R" THEN DO:        /** RECEIPTS **/
        {rm/rm-post.i "rm-bin.qty" "rm-bin.cost" "rm-rctd.qty" "rm-rctd.cost"}

        ASSIGN
         rm-bin.qty     = rm-bin.qty + ld-cvt-qty
         item.last-cost = rm-rctd.cost
         item.q-onh     = item.q-onh + ld-cvt-qty.

        {rm/rm-poupd.i 2}

        item.q-avail = item.q-onh + item.q-ono - item.q-comm.
      END. /* R */

      ELSE
      IF rm-rctd.rita-code EQ "I" THEN DO:  /** ISSUES **/

         IF rm-rctd.tag NE "" THEN
            FOR EACH b-rd FIELDS(r-no rita-code tag2) WHERE
                b-rd.company   EQ cocode AND
                b-rd.tag       EQ rm-rctd.tag AND
                b-rd.loc       EQ rm-rctd.loc AND
                b-rd.loc-bin   EQ rm-rctd.loc-bin AND
                b-rd.rita-code EQ "R" AND
                b-rd.tag2      NE ""
                NO-LOCK
                USE-INDEX tag,
                FIRST b-rh WHERE
                      b-rh.r-no EQ b-rd.r-no AND
                      b-rh.rita-code EQ b-rd.rita-code AND
                      b-rh.i-no      EQ rm-rctd.i-no
                      NO-LOCK:

                rm-rctd.tag2 = b-rd.tag2.
            END.

         IF AVAIL job AND job.job-no NE "" THEN DO:
            RUN rm/mkjobmat.p (RECID(rm-rctd),rm-rctd.company, OUTPUT v-recid).

            FIND job-mat WHERE RECID(job-mat) EQ v-recid NO-ERROR.

            IF NOT AVAIL job-mat THEN DO:
               BELL.
               MESSAGE " Job Mat Record not found for "
                       STRING(job.job-no + "-" + string(job.job-no2,"99") +
                              "  " + rm-rctd.i-no)
                       VIEW-AS ALERT-BOX.
               UNDO transblok, NEXT transblok.
            END.

            ASSIGN
             v-bwt = job-mat.basis-w
             v-len = job-mat.len
             v-wid = job-mat.wid
             v-dep = item.s-dep.

            IF v-len EQ 0 THEN v-len = item.s-len.

            IF v-wid EQ 0 THEN
              v-wid = IF item.r-wid NE 0 THEN item.r-wid ELSE item.s-wid.

            IF v-bwt EQ 0 THEN v-bwt = item.basis-w.

            IF INDEX("RL",job.stat) NE 0 THEN job.stat = "W".

            {rm/rmmatact.i}            /* Create Actual Material */

            out-qty = rm-rctd.qty.
            IF rm-rctd.pur-uom NE job-mat.qty-uom AND rm-rctd.pur-uom NE "" THEN
               RUN sys/ref/convquom.p(rm-rctd.pur-uom, job-mat.qty-uom,
                                      v-bwt, v-len, v-wid, v-dep,
                                      rm-rctd.qty, OUTPUT out-qty).

            cost = rm-rctd.cost.
            IF rm-rctd.pur-uom NE job-mat.sc-uom AND rm-rctd.pur-uom NE "" THEN
               RUN sys/ref/convcuom.p(rm-rctd.pur-uom, job-mat.sc-uom,
                                      v-bwt, v-len, v-wid, v-dep,
                                      rm-rctd.cost, OUTPUT cost).

            ASSIGN
             mat-act.qty-uom = job-mat.qty-uom
             mat-act.cost    = cost
             mat-act.qty     = mat-act.qty     + out-qty
             job-mat.qty-iss = job-mat.qty-iss + out-qty
             job-mat.qty-all = job-mat.qty-all - out-qty
             item.q-comm     = item.q-comm     - rm-rctd.qty.

            RUN sys/ref/convquom.p(rm-rctd.pur-uom, job-mat.sc-uom,
                                   v-bwt, v-len, v-wid, v-dep,
                                   rm-rctd.qty, OUTPUT out-qty).

            mat-act.ext-cost = mat-act.ext-cost + (cost * out-qty).

            /* Don't relieve more than were allocated */
            IF job-mat.qty-all LT 0 THEN DO:
              RUN sys/ref/convquom.p(job-mat.qty-uom, rm-rctd.pur-uom,
                                     v-bwt, v-len, v-wid, v-dep,
                                     job-mat.qty-all, OUTPUT out-qty).
              ASSIGN
               job-mat.qty-all = 0
               item.q-comm     = item.q-comm - out-qty.
            END.

            /*job-mat.all-flg = (job-mat.qty-all gt 0).*/
            IF item.q-comm LT 0 THEN item.q-comm = 0.

            IF item.mat-type EQ "B" THEN RUN rm/rm-addcr.p (ROWID(rm-rctd)).
         END.

         FIND FIRST rm-bin
             WHERE rm-bin.company EQ rm-rctd.company
               AND rm-bin.loc     EQ rm-rctd.loc
               AND rm-bin.i-no    EQ rm-rctd.i-no
               AND rm-bin.loc-bin EQ rm-rctd.loc-bin
               AND rm-bin.tag     EQ rm-rctd.tag
             NO-ERROR.

         ASSIGN
          rm-bin.qty     = rm-bin.qty - ld-cvt-qty
          item.q-onh     = item.q-onh - ld-cvt-qty
          item.qlast-iss = rm-rctd.qty
          item.dlast-iss = rm-rctd.rct-date
          item.q-ytd     = item.q-ytd + rm-rctd.qty
          item.q-ptd     = item.q-ptd + rm-rctd.qty
          item.u-ptd     = item.u-ptd + (rm-rctd.cost * rm-rctd.qty)
          item.u-ytd     = item.u-ytd + (rm-rctd.cost * rm-rctd.qty)
          item.q-avail   = item.q-onh + item.q-ono - item.q-comm.
          
          IF NOT RmKeepZeroBin-log AND rm-bin.qty EQ 0 THEN
            DELETE rm-bin. 
            
      END.  /* I */

      ELSE
      IF rm-rctd.rita-code EQ "A" THEN DO:  /** ADJUSTMENTS **/
        IF rm-rctd.cost NE 0 THEN DO:
          {rm/rm-post.i "rm-bin.qty" "rm-bin.cost" "rm-rctd.qty" "rm-rctd.cost"}
        END.

        ASSIGN
         rm-bin.qty     = rm-bin.qty + ld-cvt-qty
         item.last-cost = IF rm-rctd.cost NE 0 THEN rm-rctd.cost
                                               ELSE item.last-cost
         item.q-onh     = item.q-onh + ld-cvt-qty
         item.q-avail   = item.q-onh + item.q-ono - item.q-comm.
      END. /* A */

      ELSE
      IF rm-rctd.rita-code EQ "T" THEN DO:  /** TRANSFERS **/
        ASSIGN
         rm-bin.qty   = rm-bin.qty - rm-rctd.qty
         rm-rctd.cost = rm-bin.cost.

        /* This code is to handel the Transfer to quantity to increase the BIN
           using a buffer record so current rm-bin record is not updated. */

        FIND FIRST xrm-bin
             WHERE xrm-bin.company EQ rm-rctd.company
               AND xrm-bin.loc     EQ rm-rctd.loc2
               AND xrm-bin.i-no    EQ rm-rctd.i-no
               AND xrm-bin.loc-bin EQ rm-rctd.loc-bin2
               AND xrm-bin.tag     EQ rm-rctd.tag2
             NO-ERROR.
        IF NOT AVAIL xrm-bin THEN DO:
          CREATE xrm-bin.
          ASSIGN
           xrm-bin.company = rm-rctd.company
           xrm-bin.loc     = rm-rctd.loc2
           xrm-bin.loc-bin = rm-rctd.loc-bin2
           xrm-bin.tag     = rm-rctd.tag2
           xrm-bin.i-no    = rm-rctd.i-no.
        END.

        {rm/rm-post.i "xrm-bin.qty" "xrm-bin.cost" "rm-rctd.qty" "rm-rctd.cost"}

        xrm-bin.qty = xrm-bin.qty + rm-rctd.qty.
      END. /* T */

/*       /** Delete Bins With Zero Quantities. **/ */
/*       IF rm-bin.qty EQ 0 THEN DELETE rm-bin.    */

      RELEASE loadtag.
      IF TRIM(rm-rctd.tag) NE "" THEN
      FIND FIRST loadtag EXCLUSIVE-LOCK 
          WHERE loadtag.company     EQ rm-rctd.company
            AND loadtag.item-type   EQ YES
            AND loadtag.tag-no      EQ rm-rctd.tag
            AND loadtag.i-no        EQ rm-rctd.i-no
            AND loadtag.is-case-tag EQ NO
          NO-ERROR.

      IF AVAIL loadtag THEN DO:
        IF rm-rctd.rita-code EQ "T" THEN 
          ASSIGN
           loadtag.loc     = rm-rctd.loc2
           loadtag.loc-bin = rm-rctd.loc-bin2.
        ELSE
          ASSIGN
           loadtag.loc     = rm-rctd.loc
           loadtag.loc-bin = rm-rctd.loc-bin.

        li = INDEX("RI",rm-rctd.rita-code).

        IF li EQ 1 AND (NOT AVAIL rm-bin OR rm-bin.qty LT 0) THEN li = 3.

        IF li GT 0 THEN loadtag.sts = ENTRY(li,"Received,Issued,Deleted").
      END.

      IF LAST-OF(tt-rctd.i-no) THEN             /* Calculate average cost */
      FOR EACH rm-bin
          WHERE rm-bin.company EQ rm-rctd.company
            AND rm-bin.i-no    EQ rm-rctd.i-no
          NO-LOCK USE-INDEX i-no
          BREAK BY rm-bin.i-no:

        IF FIRST(rm-bin.i-no) THEN
          ASSIGN
           v-i-qty = 0
           cost    = 0.

        v-r-qty = rm-bin.qty.

        IF v-r-qty LT 0 THEN v-r-qty = v-r-qty * -1.

        ASSIGN
         v-i-qty = v-i-qty + v-r-qty
         cost    = cost    + (v-r-qty * rm-bin.cost).

        IF cost EQ ? THEN cost = 0.

        IF LAST(rm-bin.i-no) AND v-i-qty NE 0 AND cost NE 0  
            AND v-i-qty NE ? AND cost NE ? THEN 
          item.avg-cost = cost / v-i-qty.

      END. /* each rm-bin */      

      /* gdm - 10280903 - Assign prep code received date */
      RUN assign-prep-info. 
      RUN final-steps.

      FIND CURRENT rm-rctd NO-LOCK NO-ERROR.
      FIND CURRENT item NO-LOCK NO-ERROR.
      FIND CURRENT loadtag NO-LOCK NO-ERROR.
      FIND CURRENT rm-rcpth NO-LOCK NO-ERROR.
      FIND CURRENT rm-rdtlh NO-LOCK NO-ERROR.
      FIND CURRENT mat-act NO-LOCK NO-ERROR.
      FIND CURRENT job NO-LOCK NO-ERROR.
      FIND CURRENT job-mat NO-LOCK NO-ERROR.
    END. /* for each rm-rctd */

    v-dunne = YES.
    FOR EACH rm-rctd
        WHERE rm-rctd.company   EQ cocode
          AND rm-rctd.rita-code EQ "ADDER"
          AND rm-rctd.job-no    GE v-from-job
          AND rm-rctd.job-no    LE v-to-job
          AND rm-rctd.job-no2    GE INTEGER(begin_job-no2)
          AND rm-rctd.job-no2    LE INTEGER(end_job-no2)
          AND ((begin_userid    LE "" AND
                end_userid      GE "") OR
               (rm-rctd.user-id GE begin_userid AND
                rm-rctd.user-id LE end_userid))
        TRANSACTION:

      rm-rctd.rita-code = "I".
    END.     

    IF rmpostgl THEN DO TRANSACTION:
      /* gdm - 11050906 */
      REPEAT:

        FIND FIRST gl-ctrl EXCLUSIVE-LOCK
          WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
        IF AVAIL gl-ctrl THEN DO:
          ASSIGN v-trnum       = gl-ctrl.trnum + 1
                 gl-ctrl.trnum = v-trnum.

          FIND CURRENT gl-ctrl NO-LOCK NO-ERROR.

          RUN gl-from-work (1, v-trnum).
          RUN gl-from-work (2, v-trnum).
          LEAVE.
        END. /* IF AVAIL gl-ctrl */
      END. /* REPEAT */
      /* gdm - 11050906 */
    END. /* IF rmpostgl */
    IF CAN-FIND(FIRST tt-email) THEN 
      RUN send-rmemail (v-rmemail-file).

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ rm/rep/rm-post.p 10/93 cd */
{sys/form/r-top3rm.f}
DEFINE OUTPUT PARAMETER oplValidQty AS LOGICAL     NO-UNDO.
DEF VAR v-type-prt AS ch FORMAT "X(11)" INIT "".
DEF VAR v-ext-cost AS de.
DEF VAR v-tot-qty AS DEC FORMAT "->>,>>>,>>9.99<<".
DEF VAR v-tot-cost AS DEC FORMAT "->,>>>>,>>9.99<<".
DEF VAR v-grd-qty AS DEC FORMAT "->>,>>>,>>9.99<<".
DEF VAR v-grd-cost AS DEC FORMAT "->,>>>>,>>9.99<<".
DEF VAR v-po-no LIKE rm-rctd.po-no.
DEF VAR v-inkissue AS LOG.
DEF VAR v-whse LIKE rm-rctd.loc.
DEF VAR v-int AS INT NO-UNDO.
DEF VAR ll-one-item AS LOG NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.
DEF VAR v-dscr          LIKE account.dscr.
DEF VAR v-disp-actnum   LIKE account.actnum.
DEF VAR v-disp-amt      AS   DEC FORMAT ">>,>>>,>>9.99cr".

DEF VAR v-itmcnt  AS INT NO-UNDO.
DEF VAR v-gitmcnt AS INT NO-UNDO.

DEF VAR v-totRita   AS CHAR                           NO-UNDO.
DEF VAR v-totLabl   AS CHAR FORMAT "x(50)"            NO-UNDO.
DEF VAR v-tot-qtyI  AS DEC  FORMAT "->>,>>>,>>9.99<<" NO-UNDO.
DEF VAR v-tot-costI AS DEC  FORMAT "->,>>>>,>>9.99<<" NO-UNDO.
DEF VAR v-grd-qtyI  AS DEC  FORMAT "->>,>>>,>>9.99<<" NO-UNDO.
DEF VAR v-grd-costI AS DEC  FORMAT "->,>>>>,>>9.99<<" NO-UNDO.
DEF VAR v-create-issue AS LOG NO-UNDO.
DEF VAR lValidQty AS LOG NO-UNDO.


DEF BUFFER b-rm-rctd FOR rm-rctd.
DEF BUFFER b-item FOR ITEM.
DEF BUFFER b-tt-rctd FOR tt-rctd.

FORM HEADER "WHSE:" v-whse WITH PAGE-TOP FRAME r-top1.

FORM tt-rctd.rct-date                   LABEL "DATE"
     tt-rctd.i-no                       LABEL "ITEM"
     tt-rctd.i-name FORMAT "x(14)"      LABEL "DESCRIPTION"
     tt-rctd.po-no                      LABEL "P.O.#"
     po-ord.vend-no                     LABEL "VENDOR"
     tt-rctd.job-no                     LABEL "Job #" SPACE(0) "-" SPACE(0)
     tt-rctd.job-no2                    LABEL ""
     tt-rctd.rita-code                  LABEL "T"
     tt-rctd.tag                        LABEL "TAG#" FORM "x(20)"
     tt-rctd.qty FORMAT "->>>>9.99<<"   LABEL "QUANTITY" 
     tt-rctd.loc-bin                    LABEL "BIN"
     tt-rctd.pur-uom                    LABEL "UOM"    
     tt-rctd.cost FORMAT "->>>>9.99"    LABEL "COST"
     v-ext-cost                         LABEL "TOTAL COST"

    WITH FRAME itemx NO-BOX DOWN STREAM-IO WIDTH 132.

FORM v-disp-actnum LABEL "G/L ACCOUNT NUMBER"
     v-dscr        LABEL "DESCRIPTION"
     udate         LABEL "DATE"   
     v-disp-amt    LABEL "AMOUNT" SKIP

    WITH DOWN STREAM-IO WIDTH 130 FRAME gldetail.

   ASSIGN v-create-issue = LOGICAL(fiAutoIssue:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
   oplValidQty = TRUE.
    FOR EACH tt-rctd:
      DELETE tt-rctd.
    END.

    FOR EACH rm-rctd 
        WHERE rm-rctd.company   EQ cocode
          AND rm-rctd.job-no    GE v-from-job
          AND rm-rctd.job-no    LE v-to-job
          AND rm-rctd.job-no2   GE INTEGER(begin_job-no2)
          AND rm-rctd.job-no2   LE INTEGER(end_job-no2)
          AND rm-rctd.rct-date  GE ldt-from
          AND rm-rctd.rct-date  LE ldt-to
          AND INDEX(v-types,rm-rctd.rita-code) GT 0 
          AND rm-rctd.rita-code NE "C"
          AND ((begin_userid    LE "" AND
                end_userid      GE "") OR
               (rm-rctd.user-id GE begin_userid AND
               rm-rctd.user-id LE end_userid))
        NO-LOCK:
      CREATE tt-rctd.
      BUFFER-COPY rm-rctd TO tt-rctd
      ASSIGN
       tt-rctd.rm-row-id = ROWID(rm-rctd)
       tt-rctd.has-rec   = YES
       tt-rctd.db-seq-no = rm-rctd.r-no 
       tt-rctd.seq-no    = 1.
    END.

    /* Validate quantity on tags */
    RUN checkIssuedQty (OUTPUT lValidQty).

    IF INDEX(v-types,"R") GT 0 THEN
    auto-issue:
    FOR EACH tt-rctd
        WHERE tt-rctd.rita-code EQ "R"
          AND tt-rctd.job-no    NE ""
        NO-LOCK,
        FIRST item
        WHERE item.company EQ cocode
          AND item.i-no    EQ tt-rctd.i-no
        NO-LOCK.

      RELEASE po-ordl.

      v-po-no = TRIM(tt-rctd.po-no).
      IF v-po-no NE "" THEN DO:
        DO x = 1 TO LENGTH(v-po-no):
          IF substr(v-po-no,x,1) LT "0" OR
             substr(v-po-no,x,1) GT "9" THEN NEXT auto-issue.
        END.

        FIND FIRST po-ordl
            WHERE po-ordl.company   EQ cocode
              AND po-ordl.i-no      EQ tt-rctd.i-no
              AND po-ordl.po-no     EQ int(v-po-no)
              AND po-ordl.job-no    EQ tt-rctd.job-no
              AND po-ordl.job-no2   EQ tt-rctd.job-no2
              AND po-ordl.item-type EQ YES
            USE-INDEX item-ordno NO-LOCK NO-ERROR.
      END.
      IF item.mat-type NE "I" OR AVAIL po-ordl THEN
        IF (item.i-code EQ "E" AND
            NOT AVAIL po-ordl)      OR
           (item.i-code EQ "R" AND
            NOT v-autoissue)        THEN NEXT auto-issue.

      EMPTY TEMP-TABLE tt-mat.

      RELEASE job.
      IF tt-rctd.job-no NE "" AND tt-rctd.s-num EQ ? THEN
      FIND FIRST job
        WHERE job.company EQ cocode
          AND job.job-no  EQ tt-rctd.job-no
          AND job.job-no2 EQ tt-rctd.job-no2
        NO-LOCK NO-ERROR.

      IF AVAIL job THEN DO:
        ld = 0.

        FOR EACH job-mat
            WHERE job-mat.company EQ job.company
              AND job-mat.job     EQ job.job
              AND job-mat.job-no  EQ job.job-no
              AND job-mat.job-no2 EQ job.job-no2
              AND job-mat.rm-i-no EQ tt-rctd.i-no
            NO-LOCK
            BY job-mat.frm:
          CREATE tt-mat.
          ASSIGN
           tt-mat.frm = job-mat.frm
           tt-mat.qty = job-mat.qty
           ld         = ld + job-mat.qty.
        END.

        FOR EACH tt-mat:
          tt-mat.qty = tt-rctd.qty * (tt-mat.qty / ld).
          IF tt-rctd.pur-uom EQ "EA" THEN DO:
            {sys/inc/roundup.i tt-mat.qty} 
          END.
        END.

        ld = 0.
        FOR EACH tt-mat:
          ld = ld + tt-mat.qty.
        END.

        IF ld NE tt-rctd.qty THEN
        FOR EACH tt-mat:
          tt-mat.qty = tt-mat.qty + (tt-rctd.qty - ld).
          LEAVE.
        END.
      END.

      ELSE DO:
        CREATE tt-mat.
        ASSIGN
         tt-mat.frm = tt-rctd.s-num
         tt-mat.qty = tt-rctd.qty.
      END.

      IF v-create-issue OR (item.i-code EQ "E" AND tt-rctd.tag EQ "") THEN DO:
        FOR EACH tt-mat:

          CREATE b-tt-rctd.
          BUFFER-COPY tt-rctd EXCEPT rec_key TO b-tt-rctd
          ASSIGN
            b-tt-rctd.rita-code = "I"
            b-tt-rctd.tt-row-id = ROWID(tt-rctd)
            b-tt-rctd.seq-no    = 2
            b-tt-rctd.s-num     = tt-mat.frm
            b-tt-rctd.qty       = tt-mat.qty .
            IF AVAIL po-ordl THEN do:
             FIND FIRST po-ord NO-LOCK 
                    WHERE po-ord.company EQ po-ordl.company 
                      AND po-ord.po-no EQ po-ordl.po-no NO-ERROR.
            
             IF AVAIL po-ord AND po-ord.TYPE <> "S" THEN
                 ASSIGN b-tt-rctd.po-no     = ""  .
            END.
          DELETE tt-mat.
        END. /* FOR EACH tt-mat */

      END. /* IF v-create-issue  */

    END. /* if index(v-types,"R") gt 0 */

    issue-adder-for-board:
    FOR EACH tt-rctd
        WHERE tt-rctd.rita-code EQ "I"
          AND tt-rctd.job-no    NE ""
        NO-LOCK,
        FIRST job
        WHERE job.company EQ cocode
          AND job.job-no  EQ tt-rctd.job-no
          AND job.job-no2 EQ tt-rctd.job-no2
        NO-LOCK,

        FIRST item
        WHERE item.company  EQ cocode
          AND item.i-no     EQ tt-rctd.i-no
          AND item.mat-type EQ "B"
        NO-LOCK:
         IF AVAIL b-tt-rctd THEN DO:
               {rm/rm-addcr.i E b-tt-rctd b-tt-rctd b-}
        ASSIGN
         b-tt-rctd.tt-row-id = ROWID(tt-rctd)
         b-tt-rctd.seq-no    = 3.
         END.
      END.
    END.
    ASSIGN
     str-tit2 = c-win:TITLE
     {sys/inc/ctrtext.i str-tit2 112}.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    DISPLAY "" WITH FRAME r-top.

  IF tb_excel THEN
  DO:
      OUTPUT STREAM excel TO VALUE(fi_file).
      ASSIGN excelheader = "DATE,ITEM,DESCRIPTION,PO #,VENDOR,JOB #,JOB #2,T,TAG #,QUANTITY,BIN,UOM,COST,TOTAL COST".
      PUT STREAM excel UNFORMATTED excelheader SKIP.
  END.
    ASSIGN
     v-grd-qty  = 0
     v-grd-cost = 0.

    FOR EACH tt-rctd WHERE INDEX(v-types,tt-rctd.rita-code) GT 0 
        BREAK BY tt-rctd.loc                                             
              BY tt-rctd.i-no                                            
              BY tt-rctd.job-no                                          
              BY tt-rctd.job-no2 
              BY tt-rctd.loc-bin                           
              BY tt-rctd.tag
              BY RECID(tt-rctd)

        WITH FRAME itemx:                                                   

      IF FIRST-OF(tt-rctd.loc) THEN DO:
        v-whse = tt-rctd.loc.

        IF FIRST(tt-rctd.loc) THEN DISPLAY WITH FRAME r-top1.

        ELSE PUT SKIP(3) "WHSE: " v-whse.
      END.

      FIND FIRST item NO-LOCK
          WHERE item.company EQ cocode
            AND item.i-no    EQ tt-rctd.i-no
          NO-ERROR.

      RELEASE costtype.
      IF AVAIL item THEN
      FIND FIRST costtype NO-LOCK
          WHERE costtype.company   EQ cocode
            AND costtype.cost-type EQ item.cost-type
          NO-ERROR.

      RELEASE po-ord.
      IF int(tt-rctd.po-no) NE 0 AND tt-rctd.rita-code EQ "R" THEN                                         
      FIND FIRST po-ord NO-LOCK
          WHERE po-ord.company EQ cocode
            AND po-ord.po-no   EQ int(tt-rctd.po-no)
          NO-ERROR.

      RELEASE po-ordl.
      IF AVAIL po-ord THEN
      FIND FIRST po-ordl NO-LOCK
          WHERE po-ordl.company   EQ cocode
            AND po-ordl.po-no     EQ po-ord.po-no
            AND po-ordl.i-no      EQ tt-rctd.i-no
            AND po-ordl.job-no    EQ tt-rctd.job-no
            AND po-ordl.job-no2   EQ tt-rctd.job-no2
            AND po-ordl.s-num     EQ tt-rctd.s-num
            AND po-ordl.b-num     EQ tt-rctd.b-num
            AND po-ordl.deleted   EQ NO
            AND po-ordl.item-type EQ YES
          NO-ERROR.

      v-ext-cost = tt-rctd.cost * tt-rctd.qty.

      IF rmpostgl AND AVAIL costtype AND costtype.inv-asset NE ""  AND
         v-ext-cost NE 0 AND v-ext-cost NE ?                       THEN DO:

        IF tt-rctd.rita-code EQ "R"  AND  
           costtype.ap-accrued NE "" THEN DO:

          /* Debit RM Asset */
          FIND FIRST work-gl WHERE work-gl.actnum EQ costtype.inv-asset NO-LOCK NO-ERROR.
          IF NOT AVAIL work-gl THEN DO:
            CREATE work-gl.
            work-gl.actnum = costtype.inv-asset.
          END.
          work-gl.debits = work-gl.debits + v-ext-cost.

          /* Credit RM AP Accrued */
          FIND FIRST work-gl WHERE work-gl.actnum EQ costtype.ap-accrued NO-LOCK NO-ERROR.
          IF NOT AVAIL work-gl THEN DO:
            CREATE work-gl.
            work-gl.actnum = costtype.ap-accrued.
          END.
          work-gl.credits = work-gl.credits + v-ext-cost.
        END.

        ELSE
        IF tt-rctd.rita-code EQ "I" AND
           tt-rctd.job-no NE ""     THEN DO:

          FOR EACH job-hdr
              WHERE job-hdr.company EQ cocode
                AND job-hdr.job-no  EQ tt-rctd.job-no
                AND job-hdr.job-no2 EQ tt-rctd.job-no2
              NO-LOCK,
              FIRST job OF job-hdr NO-LOCK
              BREAK BY job-hdr.frm:
            ll-one-item = FIRST(job-hdr.frm) AND LAST(job-hdr.frm).
            LEAVE.
          END.

          FOR EACH job-hdr
              WHERE job-hdr.company     EQ cocode
                AND job-hdr.job-no      EQ tt-rctd.job-no
                AND job-hdr.job-no2     EQ tt-rctd.job-no2
                AND ((job-hdr.frm       EQ tt-rctd.s-num AND
                      (job-hdr.blank-no EQ tt-rctd.b-num OR tt-rctd.b-num EQ 0))
                 OR  ll-one-item)
              NO-LOCK,
              FIRST job OF job-hdr NO-LOCK,
              FIRST itemfg
              WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ job-hdr.i-no
              NO-LOCK,
              FIRST prodl
              WHERE prodl.company EQ cocode
                AND prodl.procat  EQ itemfg.procat
                AND CAN-FIND(FIRST prod
                             WHERE prod.company EQ cocode
                               AND prod.prolin  EQ prodl.prolin)
              NO-LOCK,
              FIRST prod
              WHERE prod.company EQ cocode
                AND prod.prolin  EQ prodl.prolin
                AND prod.wip-mat NE ""
              NO-LOCK:

            ld = ROUND(v-ext-cost * (IF ll-one-item        OR
                                        tt-rctd.b-num NE 0 OR
                                        job-hdr.sq-in LE 0 OR
                                        job-hdr.sq-in EQ ? THEN 1
                                     ELSE (job-hdr.sq-in / 100)),2).

            /* Debit FG Wip Material */
            FIND FIRST work-gl
                WHERE work-gl.job     EQ job-hdr.job
                  AND work-gl.job-no  EQ job-hdr.job-no
                  AND work-gl.job-no2 EQ job-hdr.job-no2
                  AND work-gl.actnum  EQ prod.wip-mat 
                NO-LOCK NO-ERROR.
            IF NOT AVAIL work-gl THEN DO:
              CREATE work-gl.
              ASSIGN
               work-gl.job     = job-hdr.job
               work-gl.job-no  = job-hdr.job-no
               work-gl.job-no2 = job-hdr.job-no2
               work-gl.actnum  = prod.wip-mat.
            END.
            work-gl.debits = work-gl.debits + ld.

            /* Credit RM Asset */
            FIND FIRST work-gl
                WHERE work-gl.job     EQ job-hdr.job
                  AND work-gl.job-no  EQ job-hdr.job-no
                  AND work-gl.job-no2 EQ job-hdr.job-no2
                  AND work-gl.actnum  EQ costtype.inv-asset
                NO-LOCK NO-ERROR.
            IF NOT AVAIL work-gl THEN DO:
              CREATE work-gl.
              ASSIGN
               work-gl.job     = job-hdr.job
               work-gl.job-no  = job-hdr.job-no
               work-gl.job-no2 = job-hdr.job-no2
               work-gl.actnum  = costtype.inv-asset.
            END.
            work-gl.credits = work-gl.credits + ld.
          END.
        END.
      END.

      DISPLAY tt-rctd.rct-date   WHEN FIRST-OF(tt-rctd.i-no)
              tt-rctd.i-no       WHEN FIRST-OF(tt-rctd.i-no)
              tt-rctd.i-name     WHEN FIRST-OF(tt-rctd.i-no)
              tt-rctd.po-no
              po-ord.vend-no     WHEN AVAIL po-ord                     
              tt-rctd.job-no
              tt-rctd.job-no     WHEN tt-rctd.job-no EQ "" @ tt-rctd.job-no
              tt-rctd.job-no2
              tt-rctd.job-no2    WHEN tt-rctd.job-no EQ "" @ tt-rctd.job-no2
              tt-rctd.rita-code
              tt-rctd.tag
              tt-rctd.qty
              tt-rctd.loc-bin
              tt-rctd.pur-uom                                   
              tt-rctd.cost
              v-ext-cost.
      DOWN.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
              (IF FIRST-OF(tt-rctd.i-no) THEN tt-rctd.rct-date ELSE ?) ","
              (IF FIRST-OF(tt-rctd.i-no) THEN tt-rctd.i-no ELSE "" )     ","
              (IF FIRST-OF(tt-rctd.i-no) THEN tt-rctd.i-name ELSE "")   ","
              tt-rctd.po-no                                             ","
              (IF AVAIL po-ord THEN po-ord.vend-no ELSE "")             ","
              (IF tt-rctd.job-no <> "" THEN tt-rctd.job-no ELSE "" )    ","
              (IF tt-rctd.job-no EQ "" THEN tt-rctd.job-no2 ELSE 0 ) FORMAT "99"  ","
              tt-rctd.rita-code                                  ","
              tt-rctd.tag                                        ","
              tt-rctd.qty                                        ","
              tt-rctd.loc-bin                                    ","
              tt-rctd.pur-uom                                    ","
              tt-rctd.cost                                       ","
              v-ext-cost  SKIP .


      IF tt-rctd.rita-code EQ "T" THEN 
        PUT "To WHSE: " AT 69
            tt-rctd.loc2 FORMAT "x(5)"
            SPACE(1)
            tt-rctd.loc-bin2
            SKIP.               

      IF tt-rctd.rita-code EQ "R" OR
         tt-rctd.rita-code EQ "A"
        THEN ASSIGN v-tot-qty = v-tot-qty + tt-rctd.qty
                    v-tot-cost = v-tot-cost + (tt-rctd.cost * tt-rctd.qty).
      IF tt-rctd.rita-code EQ "I" 
        THEN ASSIGN v-tot-qtyI  = v-tot-qtyI  + tt-rctd.qty
                    v-tot-costI = v-tot-costI + (tt-rctd.cost * tt-rctd.qty).
      ASSIGN v-itmcnt  = v-itmcnt  + 1
             v-gitmcnt = v-gitmcnt + 1.

      IF LAST-OF(tt-rctd.i-no) THEN DO:       

        IF v-pr-tots THEN DO:

          PUT "---------"                           AT 90
              "---------"                           AT 113 SKIP.

          IF t-receipt THEN 
            PUT "Total for Receipts and Adjustments:" TO 59
                v-tot-qty                             TO 98
                v-tot-cost                            TO 121 SKIP.
          IF t-issue   THEN 
            PUT "Total for Issues:                  " TO 59
                v-tot-qtyI                            TO 98
                v-tot-costI                           TO 121 SKIP.

          PUT "Total Number of Lines to Post:     " TO 59
              v-itmcnt                              TO 98  SKIP(1). 
        END.

        ASSIGN
         v-grd-qty   = v-grd-qty   + v-tot-qty 
         v-grd-cost  = v-grd-cost  + v-tot-cost
         v-grd-qtyI  = v-grd-qtyI  + v-tot-qtyI 
         v-grd-costI = v-grd-costI + v-tot-costI
         v-tot-qty   = 0
         v-tot-cost  = 0
         v-tot-qtyI  = 0
         v-tot-costI = 0
         v-itmcnt    = 0.      
      END. 
    END. /* each tt-rctd */

    IF v-pr-tots THEN DO:
      PUT "-----------"                               AT 90
          "----------"                                AT 113 SKIP.
      IF t-receipt THEN 
        PUT "Grand Total for Receipts and Adjustments:" TO 59
            v-grd-qty                                   TO 98
            v-grd-cost                                  TO 121 SKIP.
      IF t-issue   THEN
        PUT "Grand Total for Issues:                  " TO 59
            v-grd-qtyI                                  TO 98
            v-grd-costI                                 TO 121 SKIP.
        PUT "Grand Total Number of Lines to Post:     " TO 59          
             v-gitmcnt                                  TO 98 SKIP(2).
    END.

    HIDE FRAME r-top1.

    IF CAN-FIND(FIRST work-gl) THEN PAGE.

    FOR EACH work-gl BREAK BY work-gl.actnum:
      FIND FIRST account
          WHERE account.company EQ cocode
            AND account.actnum  EQ work-gl.actnum
          NO-LOCK NO-ERROR.

      ASSIGN
       v-dscr        = IF AVAIL account THEN account.dscr
                       ELSE "ACCOUNT NOT FOUND - " + work-gl.actnum
       v-disp-actnum = work-gl.actnum
       v-disp-amt    = work-gl.debits - work-gl.credits.

      DISPLAY v-disp-actnum v-dscr udate v-disp-amt
          WITH FRAME gldetail.
      DOWN WITH FRAME gldetail.
    END. /* each work-job */

    IF tb_excel THEN
    DO:
       OUTPUT STREAM excel CLOSE.
       IF tb_runExcel THEN
          OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-rmemail C-Win 
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

  FOR EACH tt-email:

      IF tt-email.total-recvd-qty < tt-email.under-qty THEN
         tt-email.undovr = "U".
      ELSE IF tt-email.total-recvd-qty > tt-email.allow-qty THEN
         tt-email.undovr = "O".
      ELSE IF rmemails = "overrun" THEN
         DELETE tt-email.
      ELSE /*rmemails = "receipts"*/
         tt-email.undovr = "R".
  END.

  FOR EACH tt-email, 
      FIRST vend WHERE
            vend.company = g_company AND
            vend.vend-no = tt-email.vend-no AND
            vend.active = "A"
            NO-LOCK
      BREAK BY tt-email.po-no: 

      IF FIRST-OF(tt-email.po-no) THEN
         ASSIGN
            lv-mailbody = ""
            v-overrun-found = NO
            v-underrun-found = NO.

      IF NOT v-overrun-found AND
         tt-email.undovr = "O" THEN
         v-overrun-found = YES.

      IF tt-email.undovr = "R" THEN  
         lv-mailbody = lv-mailbody +  CHR(10) + "Raw Material Receipt From " 
                     + "Vendor# "  + STRING (tt-email.vend-no) + "  "    
                     + "PO# " + STRING (tt-email.po-no) + "  "
                     + "Item# " + tt-email.item-no + "  "
                     + "Item Name - "  + tt-email.item-name + "  "
                     + "The Purchase Order Quantity was " + TRIM(STRING(tt-email.po-qty,"-ZZ,ZZZ,ZZ9.99")) + " "
                     + tt-email.cons-uom + ". " 
                     + "We just received " + trim(STRING(tt-email.recvd-qty,"-ZZ,ZZZ,ZZ9.99"))
                     + " " + tt-email.cons-uom  + " out of a total receipt quantity of "
                     + trim(STRING(tt-email.total-recvd-qty,"-ZZ,ZZZ,ZZ9.99")) + " " + tt-email.cons-uom
                     + ".".

      ELSE IF tt-email.undovr = "U" THEN 
         lv-mailbody = lv-mailbody + CHR(10) + "UNDERRUN WARNING - "  
                     + "Raw Material Receipt From "   
                     + "Vendor# "  + STRING (tt-email.vend-no)  + "  "     
                     + "PO# " + STRING (tt-email.po-no) + "  "
                     + "For Item# "    + tt-email.item-no       + "  "
                     + "With Item Name "  + tt-email.item-name  + "  "
                     + "The Purchase Order Quantity of " + STRING(tt-email.po-qty,"-ZZ,ZZZ,ZZ9.99")
                     + " " + tt-email.cons-uom + " has Vendor Underrun % " + string(tt-email.underrun-pct) + ". " 
                     + "We just received " + string(tt-email.recvd-qty,"-ZZ,ZZZ,ZZ9.99")
                     + " " + tt-email.cons-uom + " out of a total receipt quantity of "
                     + trim(STRING(tt-email.total-recvd-qty,"-ZZ,ZZZ,ZZ9.99"))
                     + " " + tt-email.cons-uom + ".".

      ELSE IF tt-email.undovr = "O" THEN 
         lv-mailbody = lv-mailbody + CHR(10) + "OVERRUN WARNING - "   
                     + "Raw Material Receipt From "     
                     + "Vendor# "  + STRING (tt-email.vend-no) + "  "     
                     + "PO# " + STRING (tt-email.po-no) + "  "
                     + "For Item# "    + tt-email.item-no      + "  " 
                     + "With Item Name " + tt-email.item-name  + "  " 
                     + "The Purchase Order Quantity of " + TRIM(STRING(tt-email.po-qty,"-ZZ,ZZZ,ZZ9.99"))
                     + " " + tt-email.cons-uom 
                     + " has Maximum Vendor Overrun % " + string(tt-email.overrun-pct) + ". "
                     + "We just received " + trim(STRING(tt-email.recvd-qty,"-ZZ,ZZZ,ZZ9.99"))
                     + " " + tt-email.cons-uom + " out of a total receipt quantity of "
                     + trim(STRING(tt-email.total-recvd-qty,"-ZZ,ZZZ,ZZ9.99")) + " " + tt-email.cons-uom
                     + ". " 
                     + "Allowed Qty " + trim(STRING(tt-email.allow-qty,"-ZZ,ZZZ,ZZ9.99"))
                     + " " + tt-email.cons-uom + ". "
                     + "QTY IS OVERRUN NOT AUTHORIZED.  "  + " "
                     + "NEGATIVE RECEIPT SHOULD BE ISSUED FOR OVERRUN. ".

      IF LAST-OF(tt-email.po-no) THEN DO:

          {custom/emailList.i &recKey=vend.rec_key &emailList=ls-to-list}
          {custom/emailList.i &recKey=v-cust-rec-key &emailList=ls-to-cust}      

          IF ls-to-list + ls-to-cust NE '' THEN DO:

            ASSIGN lv-mailbody = LEFT-TRIM(lv-mailbody)
                   lv-mailto = "To:" + ls-to-list + "," + ls-to-cust
                   lv-mailsubject = "".

            IF v-overrun-found THEN
               lv-mailsubject = "OVERRUN WARNING ".
            IF v-underrun-found THEN
               lv-mailsubject = "UNDERRUN WARNING ".

            lv-mailsubject = lv-mailsubject + "Raw Goods Receipts have been posted".

            RUN mail(lv-mailto,lv-mailsubject,lv-mailbody,"",
                     INT(rmemail-dlg-box),OUTPUT retcode).

          END.
      END. /* last-of(tt-email.vend-no) */
  END.

  EMPTY TEMP-TABLE tt-email.




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
  DEF VAR lv-frame-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-field2-hdl AS HANDLE NO-UNDO.
  DEF VAR parm-fld-list AS cha NO-UNDO.
  DEF VAR parm-lbl-list AS cha NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR lv-label AS cha.

  ASSIGN
  lv-frame-hdl = FRAME {&frame-name}:HANDLE
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD
  lv-field-hdl = lv-group-hdl:FIRST-CHILD.

  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
        THEN DO:
           IF lv-field-hdl:LABEL <> ? THEN 
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + ",".
           ELSE DO:  /* radio set */
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
              REPEAT:
                  IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                  IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
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
       entry(i,parm-lbl-list) NE "" THEN DO:

      lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                 trim(ENTRY(i,parm-lbl-list)) + ":".

      PUT lv-label FORMAT "x(35)" AT 5
          SPACE(1)
          TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
          SKIP.              
    END.
  END.

  PUT FILL("-",80) FORMAT "x(80)" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION is-run-from-ss C-Win 
FUNCTION is-run-from-ss RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


DEF VAR ipcPgm AS CHAR NO-UNDO.
DEF VAR oplFound AS LOG NO-UNDO.
DEF VAR lcPgmList AS CHAR NO-UNDO.
DEFINE VARIABLE hProc AS HANDLE NO-UNDO.

oplFound = NO.
ipcPgm = "addon".

lcPgmList = PROGRAM-NAME(1) 
        + (IF PROGRAM-NAME(2) NE ? THEN "," + PROGRAM-NAME(2) ELSE "")
        + (IF PROGRAM-NAME(3) NE ? THEN "," + PROGRAM-NAME(3) ELSE "")
        + (IF PROGRAM-NAME(4) NE ? THEN "," + PROGRAM-NAME(4) ELSE "")
        + (IF PROGRAM-NAME(5) NE ? THEN "," + PROGRAM-NAME(5) ELSE "")
        + (IF PROGRAM-NAME(6) NE ? THEN "," + PROGRAM-NAME(6) ELSE "")
        + (IF PROGRAM-NAME(7) NE ? THEN "," + PROGRAM-NAME(7) ELSE "").
IF INDEX(lcPgmList, ipcPgm) GT 0 THEN
    oplFound = TRUE.

RETURN oplFound.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

