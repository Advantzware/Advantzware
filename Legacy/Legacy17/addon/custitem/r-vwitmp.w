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
&IF DEFINED(UIB_IS_RUNNING) NE 0 &THEN
DEF VAR ip-post AS LOG INIT NO NO-UNDO.
DEF VAR ip-trans-type LIKE vend-whse-trans.trans-type NO-UNDO.
&ELSE
DEF INPUT PARAMETER ip-post AS LOG NO-UNDO.
DEF INPUT PARAMETER ip-trans-type LIKE vend-whse-trans.trans-type NO-UNDO.
&ENDIF

IF INDEX(PROGRAM-NAME(1),"custitem/r-vwitmp.w") NE 0 THEN
   ip-post = YES.

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

DEF TEMP-TABLE tt-vend-whse-trans LIKE vend-whse-trans 
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
FI-beg-usage-date FI-end-usage-date FI-beg-vend-code FI-end-vend-code ~
FI-beg-vend-plant-code FI-end-vend-plant-code FI-beg-user-id FI-end-user-id ~
lv-ornt lines-per-page rd-dest lv-font-no td-show-parm tb_excel tb_runExcel ~
fi_file Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS FI-beg-usage-date FI-end-usage-date ~
FI-beg-vend-code FI-end-vend-code FI-beg-vend-plant-code ~
FI-end-vend-plant-code FI-beg-user-id FI-end-user-id lv-ornt lines-per-page ~
rd-dest lv-font-no td-show-parm lv-font-name tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE FI-beg-usage-date AS DATE FORMAT "99/99/99":U 
     LABEL "Usage Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-beg-user-id AS CHARACTER FORMAT "X(8)":U 
     LABEL "User ID" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-beg-vend-code AS CHARACTER FORMAT "X(8)":U 
     LABEL "Vendor Code" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-beg-vend-plant-code AS CHARACTER FORMAT "X(8)":U 
     LABEL "Vendor Plant Code" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-end-usage-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/49 
     LABEL "Usage Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-end-user-id AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "User ID" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

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

DEFINE VARIABLE tran-period AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE v-post-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Post Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

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
     SIZE 20 BY 4.76 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 12.14.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 5.48.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 5.48.

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
     v-post-date AT ROW 2.67 COL 22.8 COLON-ALIGNED
     tran-period AT ROW 2.67 COL 63 COLON-ALIGNED
     FI-beg-usage-date AT ROW 5.05 COL 24 COLON-ALIGNED WIDGET-ID 10
     FI-end-usage-date AT ROW 5.05 COL 68 COLON-ALIGNED WIDGET-ID 18
     FI-beg-vend-code AT ROW 6.14 COL 24 COLON-ALIGNED WIDGET-ID 2
     FI-end-vend-code AT ROW 6.14 COL 68 COLON-ALIGNED WIDGET-ID 14
     FI-beg-vend-plant-code AT ROW 7.1 COL 24 COLON-ALIGNED WIDGET-ID 6
     FI-end-vend-plant-code AT ROW 7.1 COL 68 COLON-ALIGNED WIDGET-ID 16
     FI-beg-user-id AT ROW 8.14 COL 24 COLON-ALIGNED HELP
          "Enter the Beginning User ID"
     FI-end-user-id AT ROW 8.14 COL 68 COLON-ALIGNED HELP
          "Enter the Beginning User ID" WIDGET-ID 20
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
     "Ending:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 4 COL 48.2 WIDGET-ID 28
     "Beginning:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 4.05 COL 5.2 WIDGET-ID 26
     "Selection Parameters" VIEW-AS TEXT
          SIZE 24 BY .95 AT ROW 1.48 COL 3
          BGCOLOR 3 
     "Output Destination" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 13.38 COL 2
     RECT-18 AT ROW 1 COL 1
     RECT-6 AT ROW 13.05 COL 1.2
     RECT-19 AT ROW 4.38 COL 4.2 WIDGET-ID 22
     RECT-20 AT ROW 4.38 COL 46.8 WIDGET-ID 24
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 92.8 BY 22.81.


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
         TITLE              = "Usage Post"
         HEIGHT             = 22.81
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

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-F
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tran-period:HIDDEN IN FRAME FRAME-F           = TRUE.

/* SETTINGS FOR FILL-IN v-post-date IN FRAME FRAME-F
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       v-post-date:HIDDEN IN FRAME FRAME-F           = TRUE
       v-post-date:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Usage Post */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Usage Post */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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

   FOR EACH tt-vend-whse-trans:
      DELETE tt-vend-whse-trans.
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
/*                              &END_cust=v-to-job             */
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
/*                                                             */
/*            END.                                             */
/*        END.                                                 */
      WHEN 6 THEN RUN output-to-port.
   END CASE.

   IF ip-post THEN DO: 
      lv-post = CAN-FIND(FIRST tt-vend-whse-trans WHERE tt-vend-whse-trans.has-rec).

      IF lv-post = TRUE THEN
         FOR EACH tt-vend-whse-trans WHERE tt-vend-whse-trans.has-rec
                                       AND NOT CAN-FIND(FIRST vend-whse-trans WHERE ROWID(vend-whse-trans) EQ tt-vend-whse-trans.row-id):
            lv-post = NO.
            LEAVE.
         END.

      IF lv-post = TRUE THEN DO:
         lv-post = NO.
         MESSAGE "Post Transactions?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE lv-post.
      END.
      ELSE 
         MESSAGE "Sorry, nothing is available for posting..."
             VIEW-AS ALERT-BOX.

      IF lv-post = TRUE THEN DO:
         RUN post-vend-whse-trans.
         IF lv-post THEN 
            MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
         ELSE 
            MESSAGE "Posting Incomplete..." VIEW-AS ALERT-BOX ERROR.
      END.
   END.
   RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-beg-usage-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-beg-usage-date C-Win
ON LEAVE OF FI-beg-usage-date IN FRAME FRAME-F /* Usage Date */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-beg-user-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-beg-user-id C-Win
ON LEAVE OF FI-beg-user-id IN FRAME FRAME-F /* User ID */
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


&Scoped-define SELF-NAME FI-end-usage-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-end-usage-date C-Win
ON LEAVE OF FI-end-usage-date IN FRAME FRAME-F /* Usage Date */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-end-user-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-end-user-id C-Win
ON LEAVE OF FI-end-user-id IN FRAME FRAME-F /* User ID */
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


&Scoped-define SELF-NAME v-post-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-post-date C-Win
ON LEAVE OF v-post-date IN FRAME FRAME-F /* Post Date */
DO:
   ASSIGN {&self-name}.
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
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

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

  IF NOT ip-post THEN DISABLE v-post-date WITH FRAME {&FRAME-NAME}.

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}

     ASSIGN
      FI-beg-usage-date:SCREEN-VALUE = STRING(TODAY) 
      FI-beg-user-id:SCREEN-VALUE = USERID("nosweat")
      FI-end-user-id:SCREEN-VALUE   = USERID("nosweat").

    APPLY "entry" TO FI-beg-usage-date.
  END.
  {methods/nowait.i}

    {methods/setButton.i Btn_Cancel "Cancel"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i Btn_OK "OK"} /* added by script _nonAdm1Images1.p */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
   ll-valid = YES.

   FIND FIRST period WHERE period.company = cocode
                       AND period.pst     LE v-post-date
                       AND period.pend    GE v-post-date NO-LOCK NO-ERROR.
   IF AVAIL period THEN 
      tran-period:SCREEN-VALUE = STRING(period.pnum).
   ELSE
      IF ip-post THEN DO:
         MESSAGE "No Defined Period Exists for" v-post-date 
            VIEW-AS ALERT-BOX ERROR.
         ll-valid = NO.
      END.
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
  DISPLAY FI-beg-usage-date FI-end-usage-date FI-beg-vend-code FI-end-vend-code 
          FI-beg-vend-plant-code FI-end-vend-plant-code FI-beg-user-id 
          FI-end-user-id lv-ornt lines-per-page rd-dest lv-font-no td-show-parm 
          lv-font-name tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-F IN WINDOW C-Win.
  ENABLE RECT-18 RECT-6 RECT-19 RECT-20 FI-beg-usage-date FI-end-usage-date 
         FI-beg-vend-code FI-end-vend-code FI-beg-vend-plant-code 
         FI-end-vend-plant-code FI-beg-user-id FI-end-user-id lv-ornt 
         lines-per-page rd-dest lv-font-no td-show-parm tb_excel tb_runExcel 
         fi_file Btn_OK Btn_Cancel 
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
/*   DEF BUFFER b-tt-rctd FOR tt-rctd.                                          */
/*   DEF BUFFER rec-rm-rdtlh FOR rm-rdtlh.                                      */
/*   DEF BUFFER rec-rm-rcpth FOR rm-rcpth.                                      */
/*                                                                              */
/*   DEF VAR v-int AS INT NO-UNDO.                                              */
/*   DEF VAR v-qty-received AS DEC NO-UNDO.                                     */
/*                                                                              */
/*   IF rm-rctd.rita-code EQ "I" AND TRIM(rm-rctd.tag) NE "" THEN               */
/*      FOR EACH rec-rm-rdtlh NO-LOCK                                           */
/*          WHERE rec-rm-rdtlh.company   EQ rm-rctd.company                     */
/*            AND rec-rm-rdtlh.tag       EQ rm-rctd.tag                         */
/*            AND rec-rm-rdtlh.rita-code EQ "R"                                 */
/*          USE-INDEX tag,                                                      */
/*          FIRST rec-rm-rcpth                                                  */
/*          WHERE rec-rm-rcpth.r-no      EQ rec-rm-rdtlh.r-no                   */
/*            AND rec-rm-rdtlh.rita-code EQ rec-rm-rdtlh.rita-code              */
/*          NO-LOCK:                                                            */
/*                                                                              */
/*        IF rm-rctd.po-no EQ "" THEN rm-rctd.po-no = rec-rm-rcpth.po-no.       */
/*                                                                              */
/*        IF rm-rctd.job-no EQ "" THEN                                          */
/*          ASSIGN                                                              */
/*           rm-rctd.job-no = rec-rm-rcpth.job-no                               */
/*           rm-rctd.job-no2 = rec-rm-rcpth.job-no2.                            */
/*                                                                              */
/*        LEAVE.                                                                */
/*      END.                                                                    */
/*                                                                              */
/*   {rm/rm-rctd.i rm-rcpth rm-rdtlh rm-rctd} /* Create History Records */      */
/*                                                                              */
/*   IF rm-rctd.rita-code eq "R" then                                           */
/*   DO:                                                                        */
/*     {rm/rmemails.i}                                                          */
/*   END.                                                                       */
/*                                                                              */
/*   DELETE rm-rctd.                                                            */
/*                                                                              */
/*   FOR EACH b-tt-rctd WHERE b-tt-rctd.tt-row-id EQ ROWID(tt-rctd):            */
/*     v-int = 0.                                                               */
/*     FIND LAST rm-rctd NO-LOCK USE-INDEX rm-rctd NO-ERROR.                    */
/*     IF AVAIL rm-rctd AND rm-rctd.r-no GT v-int THEN v-int = rm-rctd.r-no.    */
/*     FIND LAST rm-rcpth NO-LOCK USE-INDEX r-no NO-ERROR.                      */
/*     IF AVAIL rm-rcpth AND rm-rcpth.r-no GT v-int THEN v-int = rm-rcpth.r-no. */
/*                                                                              */
/*     CREATE rm-rctd.                                                          */
/*     BUFFER-COPY b-tt-rctd TO rm-rctd                                         */
/*     ASSIGN                                                                   */
/*      rm-rctd.r-no        = v-int + 1                                         */
/*      b-tt-rctd.r-no      = rm-rctd.r-no                                      */
/*      b-tt-rctd.has-rec   = YES                                               */
/*      b-tt-rctd.rm-row-id = ROWID(rm-rctd).                                   */
/*   END.                                                                       */
/*                                                                              */
/*   DELETE tt-rctd.                                                            */


/* IF rmemails <> "NONE" THEN DO:                                                                      */
/*                                                                                                     */
/*    FIND FIRST po-ord WHERE                                                                          */
/*         po-ord.company = rm-rctd.company AND                                                        */
/*         po-ord.po-no = int(rm-rctd.po-no)                                                           */
/*         NO-LOCK NO-ERROR.                                                                           */
/*                                                                                                     */
/*    FIND FIRST po-ordl WHERE                                                                         */
/*         po-ordl.company = rm-rctd.company AND                                                       */
/*         po-ordl.po-no = int(rm-rctd.po-no) AND                                                      */
/*         po-ordl.i-no = rm-rctd.i-no                                                                 */
/*         NO-LOCK NO-ERROR.                                                                           */
/*                                                                                                     */
/*    IF AVAIL po-ord THEN                                                                             */
/*    DO:                                                                                              */
/*       FIND FIRST tt-email WHERE                                                                     */
/*            tt-email.po-no = po-ord.po-no AND                                                        */
/*            tt-email.item-no = ITEM.i-no                                                             */
/*            NO-ERROR.                                                                                */
/*                                                                                                     */
/*       IF NOT AVAIL tt-email THEN                                                                    */
/*       DO:                                                                                           */
/*          CREATE tt-email.                                                                           */
/*          ASSIGN tt-email.vend-no    =  IF AVAIL po-ord THEN po-ord.vend-no                          */
/*                                        ELSE ""                                                      */
/*                 tt-email.po-no      =  po-ord.po-no                                                 */
/*                 tt-email.item-no    =  ITEM.i-no                                                    */
/*                 tt-email.item-name  =  item.i-name                                                  */
/*                 tt-email.po-qty     =  IF AVAIL po-ordl THEN po-ordl.ord-qty                        */
/*                                        ELSE 0                                                       */
/*                 tt-email.cons-uom   =  ITEM.cons-uom.                                               */
/*                                                                                                     */
/*          IF AVAIL po-ordl AND                                                                       */
/*             ITEM.cons-uom NE po-ordl.pr-qty-uom THEN                                                */
/*             RUN sys/ref/convquom.p(po-ordl.pr-qty-uom,                                              */
/*                                    ITEM.cons-uom, ITEM.basis-w,                                     */
/*                                    po-ordl.s-len,  po-ordl.s-wid, item.s-dep,                       */
/*                                    tt-email.po-qty, OUTPUT tt-email.po-qty).                        */
/*                                                                                                     */
/*          FIND FIRST vend WHERE                                                                      */
/*               vend.company = rm-rctd.company AND                                                    */
/*               vend.vend-no = item.vend-no                                                           */
/*               NO-LOCK NO-ERROR.                                                                     */
/*                                                                                                     */
/*          ASSIGN                                                                                     */
/*             tt-email.overrun-pct = IF AVAIL po-ordl THEN po-ordl.over-pct ELSE                      */
/*                                     IF AVAIL po-ord  THEN po-ord.over-pct  ELSE                     */
/*                                     IF AVAIL vend    THEN vend.over-pct    ELSE 0                   */
/*                                                                                                     */
/*             tt-email.underrun-pct = IF AVAIL po-ordl THEN po-ordl.under-pct ELSE                    */
/*                                      IF AVAIL po-ord  THEN po-ord.under-pct  ELSE                   */
/*                                      IF AVAIL vend    THEN vend.under-pct    ELSE 0                 */
/*             tt-email.allow-qty = tt-email.po-qty + (tt-email.po-qty * tt-email.overrun-pct / 100)   */
/*             tt-email.under-qty = tt-email.po-qty - (tt-email.po-qty * tt-email.underrun-pct / 100). */
/*       END. /*NOT AVAIL tt-email*/                                                                   */
/*                                                                                                     */
/*       ASSIGN                                                                                        */
/*          tt-email.total-recvd-qty = po-ordl.t-rec-qty                                               */
/*          tt-email.recvd-qty = tt-email.recvd-qty + rm-rctd.qty.                                     */
/*                                                                                                     */
/*       IF AVAIL po-ordl THEN                                                                         */
/*       DO:                                                                                           */
/*          IF ITEM.cons-uom NE rm-rctd.pur-uom THEN                                                   */
/*             RUN sys/ref/convquom.p(rm-rctd.pur-uom,                                                 */
/*                                    ITEM.cons-uom, ITEM.basis-w,                                     */
/*                                    po-ordl.s-len,  po-ordl.s-wid, item.s-dep,                       */
/*                                    tt-email.recvd-qty, OUTPUT tt-email.recvd-qty).                  */
/*                                                                                                     */
/*          IF po-ordl.cons-uom NE ITEM.cons-uom THEN                                                  */
/*             RUN sys/ref/convquom.p(po-ordl.cons-uom,                                                */
/*                                    ITEM.cons-uom, ITEM.basis-w,                                     */
/*                                    po-ordl.s-len,  po-ordl.s-wid, item.s-dep,                       */
/*                                    tt-email.total-recvd-qty, OUTPUT tt-email.total-recvd-qty).      */
/*       END.                                                                                          */
/*    END.                                                                                             */
/* END.                                                                                                */

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
/*   DEF INPUT PARAM ip-run AS INT NO-UNDO.                                */
/*   DEF INPUT PARAM ip-trnum AS INT NO-UNDO.                              */
/*                                                                         */
/*   def var credits as dec init 0 no-undo.                                */
/*   def var debits as dec init 0 no-undo.                                 */
/*                                                                         */
/*                                                                         */
/*   FIND FIRST period                                                     */
/*       WHERE period.company EQ cocode                                    */
/*         AND period.pst     LE v-post-date                               */
/*         AND period.pend    GE v-post-date                               */
/*       NO-LOCK.                                                          */
/*                                                                         */
/*   for each work-gl                                                      */
/*       where (ip-run eq 1 and work-gl.job-no ne "")                      */
/*          or (ip-run eq 2 and work-gl.job-no eq "")                      */
/*       break by work-gl.actnum:                                          */
/*                                                                         */
/*     assign                                                              */
/*      debits  = debits  + work-gl.debits                                 */
/*      credits = credits + work-gl.credits.                               */
/*                                                                         */
/*     if last-of(work-gl.actnum) then do:                                 */
/*       create gltrans.                                                   */
/*       assign                                                            */
/*        gltrans.company = cocode                                         */
/*        gltrans.actnum  = work-gl.actnum                                 */
/*        gltrans.jrnl    = "RMPOST"                                       */
/*        gltrans.period  = period.pnum                                    */
/*        gltrans.tr-amt  = debits - credits                               */
/*        gltrans.tr-date = v-post-date                                    */
/*        gltrans.tr-dscr = if work-gl.job-no NE "" then "RM Issue to Job" */
/*                                                  else "RM Receipt"      */
/*        gltrans.trnum   = ip-trnum.                                      */
/*                                                                         */
/*       assign                                                            */
/*        debits  = 0                                                      */
/*        credits = 0.                                                     */
/*     end.                                                                */
/*   end.                                                                  */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-vend-whse-trans C-Win 
PROCEDURE post-vend-whse-trans :
DEF VAR v-rmemail-file AS cha NO-UNDO.
DEF BUFFER b-vend-whse-item FOR vend-whse-item.

SESSION:SET-WAIT-STATE ("general").

transblok:
FOR EACH tt-vend-whse-trans  
   BREAK BY tt-vend-whse-trans.seq-no
   TRANSACTION:                             

   FIND vend-whse-trans EXCLUSIVE-LOCK WHERE ROWID(vend-whse-trans) EQ tt-vend-whse-trans.row-id NO-WAIT NO-ERROR.

   IF NOT AVAIL vend-whse-trans THEN NEXT transblok.

      CREATE vend-whse-trans-hist.
      BUFFER-COPY tt-vend-whse-trans 
         EXCEPT tt-vend-whse-trans.row-id 
                tt-vend-whse-trans.has-rec 
                tt-vend-whse-trans.seq-no 
                tt-vend-whse-trans.tt-row-id
                tt-vend-whse-trans.create-userid 
                tt-vend-whse-trans.create-date 
                tt-vend-whse-trans.create-time 
                tt-vend-whse-trans.upd-date 
                tt-vend-whse-trans.upd-time 
                tt-vend-whse-trans.upd-userid



         TO vend-whse-trans-hist 
         ASSIGN 
            vend-whse-trans-hist.create-userid = USERID("NOSWEAT")
            vend-whse-trans-hist.create-date       = TODAY 
            vend-whse-trans-hist.create-time       = TIME
            vend-whse-trans-hist.upd-date          = TODAY
            vend-whse-trans-hist.upd-time          = TIME
            vend-whse-trans-hist.upd-userid = USERID("NOSWEAT") NO-ERROR.


      FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company           = vend-whse-trans.company 
                                    AND b-vend-whse-item.vendor-code       = vend-whse-trans.vendor-code 
                                    AND b-vend-whse-item.vendor-plant-code = vend-whse-trans.vendor-plant-code
                                    AND b-vend-whse-item.fg-item-no        = vend-whse-trans.cust-part-no
                                    AND b-vend-whse-item.cust-part-no      = vend-whse-trans.cust-part-no 
                                    AND b-vend-whse-item.vendor-dept-code  = vend-whse-trans.vendor-dept-code NO-ERROR.
      IF AVAILABLE(b-vend-whse-item) THEN
         IF ip-trans-type = "U" THEN
            b-vend-whse-item.plant-tot-oh-qty = b-vend-whse-item.plant-tot-oh-qty - vend-whse-trans.trans-qty.
         ELSE
            b-vend-whse-item.plant-tot-oh-qty = b-vend-whse-item.plant-tot-oh-qty + vend-whse-trans.trans-qty.

      DELETE vend-whse-trans.

/*    IF can-find(FIRST tt-email) THEN      */
/*       RUN send-rmemail (v-rmemail-file). */

    SESSION:SET-WAIT-STATE ("").
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
{sys/form/r-top3w.f}

DEF VAR v-head      AS CHAR FORMAT "x(200)" EXTENT 4.
DEF VAR v-per-rpt   AS LOG  FORMAT "PTD/YTD" INIT YES.
DEF VAR v-date      AS DATE EXTENT 10.

FORMAT HEADER
   v-head[1] SKIP
   v-head[2] SKIP
   v-head[3] SKIP
   v-head[4]
WITH FRAME r-top WIDTH 200.

ASSIGN
   str-tit3 = (IF v-per-rpt THEN "P" ELSE "Y") +
              "TD (" + STRING(v-date[1]) + "-" + STRING(v-date[2]) +
              ")"

{sys/inc/ctrtext.i str-tit3 132}

v-head[1] = "".

ASSIGN                                                                                                                              
   v-head[2] = FILL(" ",8)  +                     " Customers      Quantity   Customers    PO Customers    Suppliers       Customers  Customers  Customers  Suppliers  Suppliers   Suppliers Item     Cust. Plant"
   v-head[3] =                             "Seq. #  Usage Date          Used         PO# Line#     Part#      FG Item        A/P Code   Plant ID  Dept Code     Order#       Job#       Sell Price     On Hand Qty"
   v-head[4] = FILL("-",182).

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

DISPLAY "" WITH FRAME r-top.

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   ASSIGN excelheader = "CUST. USAGE DATE,QUANTITY USED,CUSTOMERS PO#,CUST. PO LINE#,SUPPLIERS FG ITEM,CUSTOMERS A/P CODE,CUSTOMERS PLANT ID,CUSTOMERS DEPT CODE,SUPPLIERS ORDER#,SUPPLIERS JOB#,SUPPLIERS ITEM SELL PRICE,PLANT ON-HAND QTY".
   PUT STREAM excel UNFORMATTED excelheader SKIP.
END.


FOR EACH vend-whse-trans WHERE vend-whse-trans.trans-type = ip-trans-type
                           AND vend-whse-trans.trans-date >=  FI-beg-usage-date
                           AND vend-whse-trans.trans-date <=  FI-end-usage-date
                           AND vend-whse-trans.vendor-code >= FI-beg-vend-code
                           AND vend-whse-trans.vendor-code <= FI-end-vend-code
                           AND vend-whse-trans.vendor-plant-code >= FI-beg-vend-plant-code
                           AND vend-whse-trans.vendor-plant-code <= FI-end-vend-plant-code
                           AND vend-whse-trans.upd-userid >= FI-beg-user-id 
                           AND vend-whse-trans.upd-userid <= FI-end-user-id 
   BREAK BY vend-whse-trans.trans-date                                             
         BY vend-whse-trans.vendor-code                                            
         BY vend-whse-trans.vendor-plant-code:

      CREATE tt-vend-whse-trans.
      BUFFER-COPY vend-whse-trans TO tt-vend-whse-trans
      ASSIGN
         tt-vend-whse-trans.row-id     = ROWID(vend-whse-trans)
         tt-vend-whse-trans.has-rec    = YES
         tt-vend-whse-trans.seq-no     = 1.

END.
FOR EACH tt-vend-whse-trans
  BREAK BY tt-vend-whse-trans.trans-date                                             
        BY tt-vend-whse-trans.vendor-code                                            
        BY tt-vend-whse-trans.vendor-plant-code:

   DISPLAY
      tt-vend-whse-trans.r-no FORMAT ">>>>>9"
      "  "
      tt-vend-whse-trans.trans-date           
      tt-vend-whse-trans.trans-qty
      " "
      tt-vend-whse-trans.item-po-no 
      "  "
      tt-vend-whse-trans.item-line-no
      tt-vend-whse-trans.cust-part-no
      tt-vend-whse-trans.fg-item-no
      tt-vend-whse-trans.vendor-code
      "  "
      tt-vend-whse-trans.vendor-plant-code
      " "
      tt-vend-whse-trans.vendor-dept-code
      "   "
      tt-vend-whse-trans.vend-ord-no
      " "
      TRIM(tt-vend-whse-trans.vend-job-no) + STRING(INT(tt-vend-whse-trans.vend-job-no2),"99") 
      "   "
      tt-vend-whse-trans.sell-price
      " "
      tt-vend-whse-trans.plant-tot-oh-qty
      WITH FRAME a NO-BOX NO-LABELS STREAM-IO DOWN  WIDTH 200.

   IF tb_excel THEN
      PUT STREAM excel UNFORMATTED
         '"' tt-vend-whse-trans.r-no               '",' 
         '"' tt-vend-whse-trans.trans-date         '",'         
         '"' tt-vend-whse-trans.trans-qty          '",'        
         '"' tt-vend-whse-trans.item-po-no         '",'
         '"' tt-vend-whse-trans.item-line-no       '",'
         '"' tt-vend-whse-trans.cust-part-no       '",'
         '"' tt-vend-whse-trans.fg-item-no         '",'
         '"' tt-vend-whse-trans.vendor-code        '",'
         '"' tt-vend-whse-trans.vendor-plant-code  '",'
         '"' tt-vend-whse-trans.vendor-dept-code   '",'
         '"' tt-vend-whse-trans.vend-ord-no        '",'
         '"' TRIM(tt-vend-whse-trans.vend-job-no) + STRING(INT(tt-vend-whse-trans.vend-job-no2),"99") '",'           
         '"' tt-vend-whse-trans.sell-price         '",' 
         '"' tt-vend-whse-trans.plant-tot-oh-qty   '",'
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

