&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: addon\rfq\rfqlst.w

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
{sys/inc/VAR.i "new shared"}

def var k_frac as dec init 6.25 no-undo.

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR ll-secure AS LOG NO-UNDO.

ASSIGN cocode = gcompany
       locode = gloc.

DO TRANSACTION:
   {sys/inc/rfqiso#.i}
   {sys/inc/rfqprint.i}
END.

{sys/inc/f16to32.i}
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_rfq-no end_rfq-no ~
begin_cust-no end_cust-no lv-show-detail rd-dest lv-ornt lines-per-page ~
lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_rfq-no end_rfq-no begin_cust-no ~
end_cust-no lv-show-detail rd-dest lv-ornt lines-per-page lv-font-no ~
lv-font-name td-show-parm 

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

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(256)":U 
     LABEL "Begin Customer#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rfq-no AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Begin RFQ#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(256)":U 
     LABEL "End Customer#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_rfq-no AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "End RFQ#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

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
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 19 BY 7.62 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 100 BY 9.76.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 100 BY 5.95.

DEFINE VARIABLE lv-show-detail AS LOGICAL INITIAL no 
     LABEL "Show Detail" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_rfq-no AT ROW 2.43 COL 30 COLON-ALIGNED
     end_rfq-no AT ROW 2.43 COL 63 COLON-ALIGNED
     begin_cust-no AT ROW 3.62 COL 30 COLON-ALIGNED
     end_cust-no AT ROW 3.62 COL 63 COLON-ALIGNED
     lv-show-detail AT ROW 5.05 COL 65
     rd-dest AT ROW 10.05 COL 9 NO-LABEL
     lv-ornt AT ROW 10.05 COL 37 NO-LABEL
     lines-per-page AT ROW 10.05 COL 90 COLON-ALIGNED
     lv-font-no AT ROW 13.14 COL 40 COLON-ALIGNED
     lv-font-name AT ROW 14.1 COL 34 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.48 COL 40
     btn-ok AT ROW 19.33 COL 27
     btn-cancel AT ROW 19.33 COL 61
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 9.1 COL 4
     RECT-6 AT ROW 8.86 COL 2
     RECT-7 AT ROW 1.48 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 102.4 BY 20.33.


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
         TITLE              = "Request For Quote Control"
         HEIGHT             = 20.67
         WIDTH              = 103.4
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
                                                                        */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Request For Quote Control */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Request For Quote Control */
DO:
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
      DEF VAR char-val AS cha NO-UNDO.
      DEF VAR lv-handle AS HANDLE NO-UNDO.

      CASE FOCUS:NAME :
          WHEN "Begin_rfq-no" OR WHEN "end_rfq-no" then do:
               RUN windows/l-rfqlst.w (g_company,FOCUS:SCREEN-VALUE,OUTPUT char-val).
               IF char-val <> ""  THEN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
          END.
          otherwise do:
           lv-handle = focus:handle.
           run applhelp.p.

           if g_lookup-var <> "" then do:
              lv-handle:screen-value = g_lookup-var.
           end.   /* g_lookup-var <> "" */
           g_lookup-var = "".
           apply "entry" to lv-handle.
           return no-apply.
        end.  /* otherwise */
      END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Begin Customer# */
DO:
   assign begin_cust-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rfq-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rfq-no C-Win
ON LEAVE OF begin_rfq-no IN FRAME FRAME-A /* Begin RFQ# */
DO:
   assign begin_rfq-no.
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
  assign rd-dest begin_cust-no END_cust-no begin_rfq-no END_rfq-no.

  if lv-show-detail then run run-report-detail.
  else run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_cust-no
                            &END_cust=END_cust-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END.
       when 5 then do:
          IF rfqprint-char = "Customer" THEN
             RUN email-cust-proc.
          ELSE
             RUN email-custx-proc.
       END. 
       WHEN 6 THEN run output-to-port.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* End Customer# */
DO:
   assign end_cust-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rfq-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rfq-no C-Win
ON LEAVE OF end_rfq-no IN FRAME FRAME-A /* End RFQ# */
DO:
    assign end_rfq-no.

    IF begin_rfq-no = end_rfq-no THEN DO:
       FIND FIRST rfq WHERE
            rfq.company = g_company AND
            rfq.loc  = locode AND
            rfq.rfq-no = begin_rfq-no
            NO-LOCK NO-ERROR.

       IF AVAIL rfq THEN
          ASSIGN begin_cust-no:SCREEN-VALUE = rfq.cust-no
                 end_cust-no:SCREEN-VALUE = rfq.cust-no.
   END.
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


&Scoped-define SELF-NAME lv-show-detail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-show-detail C-Win
ON return OF lv-show-detail IN FRAME FRAME-A /* Show Detail */
DO:
  apply "tab" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-show-detail C-Win
ON VALUE-CHANGED OF lv-show-detail IN FRAME FRAME-A /* Show Detail */
DO:
    assign lv-show-detail.
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  session:data-entry-return = yes.
  /*find first rfq no-lock no-error.
  if avail rfq then begin_rfq-no = rfq.rfq-no.
  */

  find last rfq USE-INDEX rfq no-lock no-error.
  if avail rfq then assign begin_rfq-no = rfq.rfq-no
                           end_rfq-no = rfq.rfq-no.
  end_cust-no = "zzzz".

  RUN enable_UI.
  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_rfq-no.
  END.

  {methods/nowait.i}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE email-cust-proc C-Win 
PROCEDURE email-cust-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF is-xprint-form THEN DO:
      RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
      {custom/asimail2.i &TYPE = "Customer"
                     &group-title='rfqlst.' 
                     &begin_cust= "begin_cust-no"
                     &end_cust= "begin_cust-no" 
                     &mail-subject=c-win:title
                     &mail-body=c-win:title
                     &mail-file=list-name }
   END.
   ELSE DO:
      {custom/asimailr2.i &TYPE = "Customer"
                          &group-title='rfqlst.'
                          &begin_cust= "begin_cust-no"
                          &end_cust= "begin_cust-no" 
                          &mail-subject=c-win:title
                          &mail-body=c-win:title
                          &mail-file=list-name }
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE email-custx-proc C-Win 
PROCEDURE email-custx-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-begin AS CHAR INIT 'A' NO-UNDO.
   DEF VAR lv-end AS CHAR INIT 'Z' NO-UNDO.

   IF is-xprint-form THEN DO:
      RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
      {custom/asimail.i &TYPE = "Customer"
                     &begin_cust=lv-begin
                     &END_cust=lv-end
                     &mail-subject="RFQ List"
                     &mail-body="RFQ List"
                     &mail-file=list-name }
   END.
   ELSE DO:
      {custom/asimailr.i &TYPE = "Customer"
                         &begin_cust=lv-begin
                         &END_cust=lv-end
                         &mail-subject="RFQ List"
                         &mail-body="RFQ List"
                         &mail-file=list-name}
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
  DISPLAY begin_rfq-no end_rfq-no begin_cust-no end_cust-no lv-show-detail 
          rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_rfq-no end_rfq-no begin_cust-no end_cust-no 
         lv-show-detail rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
         btn-ok btn-cancel 
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
     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
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
  run rfq/scr-rpt.w (list-name,"RFQ LIST BY RFQ#"). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  SESSION:SET-WAIT-STATE ("general").
  def var lv-addr1 as cha no-undo.
  def var lv-addr2 as cha no-undo.
  def var lv-city as cha no-undo.
  def var lv-tel as cha no-undo.
  def var lv-sman-name as cha no-undo.

  def var lv-chg-method as cha no-undo.
  def var lv-fob as cha no-undo.
  DEF VAR ll-is-corr-style AS LOG NO-UNDO.
  DEF VAR lv-len AS DEC NO-UNDO.
  DEF VAR lv-wid AS DEC NO-UNDO.
  DEF VAR lv-dep AS DEC NO-UNDO.
  DEF VAR lv-delimiter AS cha FORM "x" NO-UNDO.

  FIND FIRST users WHERE
     users.user_id EQ USERID("NOSWEAT")
     NO-LOCK NO-ERROR.

  IF AVAIL users AND users.user_program[2] NE "" THEN
     init-dir = users.user_program[2].
  ELSE
     init-dir = "c:\temp".

  list-name = init-dir + "\rfqlist.rpt".

  output to value(list-name) page-size 45.
  form header
     "***** RFQ List by RFQ# *****" 
     rfqiso#-char FORM "x(20)" TO 122
     "Page:" + string(page-num,">>9") form "x(10)" to 137 skip
     fill("=",137)  form "x(137)"   /* 126 for font9 (pitch 10) */
     with frame rfq-title width 137 no-box no-label stream-io.
view frame rfq-title.     
FOR EACH rfq WHERE /* rfq.company */
                   rfq.rfq-no >= begin_rfq-no and
                   rfq.rfq-no <= end_rfq-no and
                   rfq.cust-no >= begin_cust-no and
                   rfq.cust-no <= end_cust-no
                   NO-LOCK
                   ,
    EACH rfqitem OF rfq no-lock
                   BREAK BY rfq.rfq-no BY rfqitem.seq:

    if FIRST-OF(rfq.rfq-no) then do:
       find cust where cust.company = rfq.company and
                       cust.cust-no = rfq.cust-no
                       no-lock no-error. 
       assign lv-addr1 = if avail cust and rfq.cust-no <> "TEMP"
                         then cust.addr[1] else rfq.ship-addr[1]
              lv-addr2 = if avail cust and rfq.cust-no <> "TEMP" then cust.addr[2] else rfq.ship-addr[2]
              lv-city = if avail cust and rfq.cust-no <> "TEMP" then (cust.city + " " + cust.state + ", " + cust.zip)
                        else (rfq.ship-city + " " + rfq.ship-state + ", " + rfq.ship-zip) 
              lv-tel = if avail cust then "(" + cust.area-code + ")" + cust.phone  + "    Fax: " + cust.fax 
                       else "".
       if lv-addr2 = "" then assign lv-addr2 = lv-city
                                    lv-city = lv-tel
                                    lv-tel = "".
       find sman where sman.company = rfq.company and
                       sman.sman = rfq.sman
                       no-lock no-error.
       lv-sman-name = if avail sman then sman.sname else "".                                                            
       lv-fob = if rfq.fob-code = "D" then "Destination" else "Origin".

       DISPLAY   "Customer:" rfq.ship-name at 12  "   Warehouse Month:" rfq.wh-month       "                 RFQ Date     :" rfq.req-date
                  lv-addr1 at 12 form "x(30)"     "   Freight Charge: " rfq.chg-method form "x(13)" "      Required Date:" rfq.due-date    
                  lv-addr2 at 12 form "x(30)"     "   FOB Point:      " lv-fob    
                  lv-city at 12 form "x(30)"
                  lv-tel at 12 form "x(30)"       "   Sales Rep:        " rfq.sman lv-sman-name form "x(30)"  "             RFQ#:" rfq.rfq-no form ">>>>9"
            /*                
                 "RFQ#:" rfq.rfq-no form ">>>>9"    
                  "   Customer:" rfq.cust-no
                  rfq.ship-name
                  "Req-Date:" rfq.req-date
            */
                 fill("=",137) form "x(137)"  /* 126 for font9 (size 10) */ skip
 "Ln Customer Part#  FG Item#        Item Name                      Style"
 "  Length    Width    Depth Ink Coat Board      Caliper" skip
 "-- --------------- --------------- ------------------------------ -----"
 "-------- -------- -------- --- ---- ---------- -------"skip                

                  with frame rfq-hd no-labels width 137 no-box stream-io.
    end.              


    find style where style.company = rfq.company and
                      style.style = rfqitem.style
                      no-lock no-error.

    if avail style and style.industry = "2" then   ll-is-corr-style = yes.
    else ll-is-corr-style = no.

    if ll-is-corr-style AND v-cecscrn-char NE "Decimal" then 
        ASSIGN lv-len = round(trunc(rfqitem.len,0) + ((rfqitem.len - trunc(rfqitem.len,0)) / K_FRAC),2)
               lv-wid = round(trunc(rfqitem.wid,0) + ((rfqitem.wid - trunc(rfqitem.wid,0)) / K_FRAC),2)
               lv-dep = round(trunc(rfqitem.dep,0) + ((rfqitem.dep - trunc(rfqitem.dep,0)) / K_FRAC),2)
               .
    else ASSIGN lv-len = rfqitem.len
                lv-wid = rfqitem.wid
                lv-dep = rfqitem.dep.

   IF LL-IS-CORR-STYLE THEN
      PUT rfqitem.seq /*label "Ln"*/ form ">9"         
          lv-delimiter
          rfqitem.part-no /*label "Customer Part#" */
          lv-delimiter
          rfqitem.stock-no /*label "FG Item#"*/
          lv-delimiter
          rfqitem.i-name form "x(30)"
          lv-delimiter
          rfqitem.style /*label "Style"*/ form "x(5)"
          lv-delimiter
          /*rfqitem.procat*/
          lv-len FORM ">>>>9.99"
          lv-delimiter
          lv-wid FORM ">>>>9.99"
          lv-delimiter
          lv-dep FORM ">>>>9.99"
          lv-delimiter
          rfqitem.i-col /*label "Ink" */ form ">>9"
          lv-delimiter
          rfqitem.i-coat /*label "Coat"*/ form ">>>9"
          lv-delimiter
          rfqitem.board /*label "Board"*/
          lv-delimiter
          rfqitem.cal /*label "Caliper"*/ skip
          .
   ELSE PUT rfqitem.seq /*label "Ln"*/ form ">9"         
            lv-delimiter
            rfqitem.part-no /*label "Customer Part#" */
            lv-delimiter
            rfqitem.stock-no /*label "FG Item#"*/
            lv-delimiter
            rfqitem.i-name form "x(30)"
            lv-delimiter
            rfqitem.style /*label "Style"*/ form "x(5)"
            lv-delimiter
            /*rfqitem.procat*/
            lv-len FORM ">>9.9999<"
            lv-delimiter
            lv-wid FORM ">>9.9999<"
            lv-delimiter
            lv-dep FORM ">>9.9999<"
            lv-delimiter
            rfqitem.i-col /*label "Ink" */ form ">>9"
            lv-delimiter
            rfqitem.i-coat /*label "Coat"*/ form ">>>9"
            lv-delimiter
            rfqitem.board /*label "Board"*/
            lv-delimiter
            rfqitem.cal /*label "Caliper"*/ skip
            .

   disp  "Quantity:" at 7 rfqitem.qty[1 for 10] no-label 
         skip
         "Sell Price:" at 5 rfqitem.qty-price[1 for 10] form "->>,>>9.99"
         /*
         skip
      /*   "__________" "__________" "__________" "__________"
         "__________" "__________" "__________" "__________" "__________" "__________"
      */
         "----------" at 17 "----------" "----------" "----------" "----------"
         "----------" "----------" "----------" "----------" "----------" 
         */
         skip
         with frame qty no-box no-label stream-io width 132.

    if last-of(rfq.rfq-no) then do:
       disp 
            fill("-",136) form "x(136)" at 2 skip
            "Notes:" at 2 /*substring(rfq.inst,1,80) at 7 form "x(80)" skip
            substring(rfq.inst,1,80) at 7 form "x(80)" skip
            substring(rfq.inst,81,80) at 7 form "x(80)" skip
            substring(rfq.inst,161,80) at 7 form "x(80)" skip
            substring(rfq.inst,241,80) at 7 form "x(80)" skip
            substring(rfq.inst,321,80) at 7 form "x(80)" skip
            */
            rfq.inst view-as editor size 80 by 5 no-label
            fill("-",136) form "x(136)" at 2 
            with frame inst no-label no-box width 137 stream-io.
            .
       if not last(rfq.rfq-no) then page.
    end.  
END.  
put "***** End of Report *****" at 2 .
output close.


RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-detail C-Win 
PROCEDURE run-report-detail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  SESSION:SET-WAIT-STATE ("general").

  def var lv-addr1 as cha no-undo.
  def var lv-addr2 as cha no-undo.
  def var lv-city as cha no-undo.
  def var lv-tel as cha no-undo.
  def var lv-sman-name as cha no-undo.

  def var lv-chg-method as cha no-undo.
  def var lv-fob as cha no-undo.
  def var lv-last-order-no like est.ord-no no-undo.
  def var ls-label3 as cha form "x(11)" no-undo.
  def var ls-label4 as cha form "x(11)" no-undo.
  def var ls-tel as cha no-undo.
  def var ls-est-no as cha no-undo.
  def var ls-fax as cha no-undo.  
  def var li-ln-cnt as int no-undo.
  DEF VAR ll-is-corr-style AS LOG NO-UNDO.
  DEF VAR lv-len AS DEC NO-UNDO.
  DEF VAR lv-wid AS DEC NO-UNDO.
  DEF VAR lv-dep AS DEC NO-UNDO.
  DEF VAR lv-delimiter AS cha FORM "x" NO-UNDO.

  FIND FIRST users WHERE
       users.user_id EQ USERID("NOSWEAT")
       NO-LOCK NO-ERROR.

  IF AVAIL users AND users.user_program[2] NE "" THEN
     init-dir = users.user_program[2].
  ELSE
     init-dir = "c:\temp".

  list-name = init-dir + "\rfqlist.rpt".

  output to value(list-name) page-size 60.
  form header
     "***** RFQ List by RFQ# *****"  
     "(Form# 3.1-5) " at 85
      rfqiso#-char FORM "x(20)" TO 135
     "Page:" + string(page-num,">>9") form "x(10)" to 150 skip
     fill("=",152)  form "x(152)"   /* 126 for font9 (pitch 10) */
     with frame rfq-title width 152 no-box no-label stream-io page-top.

view frame rfq-title.     
FOR EACH rfq WHERE /* rfq.company */
                   rfq.rfq-no >= begin_rfq-no and
                   rfq.rfq-no <= end_rfq-no and
                   rfq.cust-no >= begin_cust-no and
                   rfq.cust-no <= end_cust-no
                   NO-LOCK
                   ,
    EACH rfqitem OF rfq no-lock
                   BREAK BY rfq.rfq-no BY rfqitem.seq:

    if FIRST-OF(rfq.rfq-no) then do:
       find cust where cust.company = rfq.company and
                       cust.cust-no = rfq.cust-no
                       no-lock no-error. 
        /*               
       ls-tel = if index(cust.phone,"-") > 0 then cust.phone else (substring(cust.phone,1,3) + "-" + substring(cust.phone,4) ).                
       ls-fax = if index(cust.fax,"-") > 0 or index(cust.fax,"(") > 0 then cust.fax 
                else "(" + substring(cust.fax,1,3) + ")" + substring(cust.fax,4,3) + "-" + substring(cust.fax,7).
       */         
       li-ln-cnt = 0.              
       assign lv-addr1 = if avail cust and rfq.cust-no <> "TEMP"
                         then cust.addr[1] else rfq.ship-addr[1]
              lv-addr2 = if avail cust and rfq.cust-no <> "TEMP" then cust.addr[2] else rfq.ship-addr[2]
              lv-city = if avail cust and rfq.cust-no <> "TEMP" then (cust.city + " " + cust.state + ", " + cust.zip)
                        else (rfq.ship-city + " " + rfq.ship-state + ", " + rfq.ship-zip) 
              /*lv-tel = if avail cust then "(" + trim(cust.area-code) + ")" + ls-tel + "    Fax: " + ls-fax 
                       else " "  */
              lv-tel = if avail cust then string(cust.area-code,"(999)") + string(cust.phone,"999-9999") + "    Fax: " + string(cust.fax,"(999)999-9999")          
                       else ""
                       .
       if lv-addr2 = "" then assign lv-addr2 = lv-city
                                    lv-city = lv-tel
                                    lv-tel = "".
       find sman where sman.company = rfq.company and
                       sman.sman = rfq.sman
                       no-lock no-error.
       lv-sman-name = if avail sman then sman.sname else "".                                                            
       lv-fob = if rfq.fob-code = "D" then "Destination" else "Origin".


       DISPLAY   "Customer:" rfq.ship-name at 12  "             Warehouse Month:" rfq.wh-month                "RFQ Date     :" at 100 rfq.req-date        "   Group#: _____________" skip
                  lv-addr1 at 12 form "x(40)"     "   Freight Charge: " rfq.chg-method form "x(13)" "Required Date:" at 100 rfq.due-date        "  Pharma#: _____________" skip
                  lv-addr2 at 12 form "x(40)"     "   FOB Point:      " lv-fob form "x(11)"         "RFQ#:       " at 100 rfq.rfq-no form ">>>>9"
                  lv-city at 12 form "x(40)"      "   Contact: _____________________________"         /* "    Estimate#:" at 100 rfqitem.est-no */ 
                  lv-tel at 12 form "x(40)"       "   Sales Rep:" rfq.sman lv-sman-name form "x(30)"  /* "  Last Order#:" at 100 lv-last-order-no  */
/*               fill("=",137) form "x(137)"  /* 126 for font9 (size 10) */ skip
 "Ln Customer Part#  FG Item#        Item Name                      Style"
 "  Length    Width    Depth Ink Coat Board      Caliper" skip
 "-- --------------- --------------- ------------------------------ -----"
 "-------- -------- -------- --- ---- ---------- -------"skip                   
 */
                  with frame rfq-hd no-labels width 150 no-box stream-io.
    end.              
    disp       fill("=",146) form "x(146)"  at 2  /* for font9 (size 10) */ skip
         "Ln Customer Part#  FG Item#        Item Name                      Style" at 2
         "  Length    Width    Depth Ink Coat Estimate#  Last Order#           PO #" skip
         "-- --------------- --------------- ------------------------------ -----" at 2
         "-------- -------- -------- --- ---- ---------- ----------- --------------"skip                   
         with frame lbl no-label no-box stream-io width 147.

     find first est where est.company = rfq.company and  est.est-no = rfqitem.est-no
                            no-lock no-error.
     lv-last-order-no = if avail est then est.ord-no else 0.              
     ls-est-no = rfqitem.est-no.

     if ls-est-no = "" then do:
          find itemfg where itemfg.company = rfq.company and
                            itemfg.i-no = rfqitem.stock-no
                            no-lock no-error.
          if avail itemfg then find first est where est.company = rfq.company and
                                                    est.est-no = itemfg.est-no
                                                    no-lock no-error.
          assign ls-est-no = if avail itemfg then itemfg.est-no else ls-est-no
                 lv-last-order-no = if avail est then est.ord-no else lv-last-order-no.
     end.     


    find style where style.company = rfq.company and
                      style.style = rfqitem.style
                      no-lock no-error.

    if avail style and style.industry = "2" then   ll-is-corr-style = yes.
    else ll-is-corr-style = no.

    if ll-is-corr-style AND v-cecscrn-char NE "Decimal" then 
        ASSIGN lv-len = round(trunc(rfqitem.len,0) + ((rfqitem.len - trunc(rfqitem.len,0)) / K_FRAC),2)
               lv-wid = round(trunc(rfqitem.wid,0) + ((rfqitem.wid - trunc(rfqitem.wid,0)) / K_FRAC),2)
               lv-dep = round(trunc(rfqitem.dep,0) + ((rfqitem.dep - trunc(rfqitem.dep,0)) / K_FRAC),2)
               .
    else ASSIGN lv-len = rfqitem.len
                lv-wid = rfqitem.wid
                lv-dep = rfqitem.dep.

   IF ll-is-corr-style THEN
       PUT rfqitem.seq /*label "Ln"*/ form ">9" at 2
           lv-delimiter
           rfqitem.part-no /*label "Customer Part#" */
           lv-delimiter
           rfqitem.stock-no /*label "FG Item#"*/
           lv-delimiter
           rfqitem.i-name form "x(30)"
           lv-delimiter
           rfqitem.style /*label "Style"*/ form "x(5)"
           lv-delimiter
           /*rfqitem.procat*/
           lv-len FORM ">>>>9.99"
           lv-delimiter
           lv-wid FORM ">>>>9.99"
           lv-delimiter
           lv-dep FORM ">>>>9.99"
           lv-delimiter
           rfqitem.i-col /*label "Ink" */ form ">>9"
           lv-delimiter
           rfqitem.i-coat /*label "Coat"*/ form ">>>9"
           lv-delimiter
           ls-est-no
           "   " lv-last-order-no skip.

   ELSE PUT rfqitem.seq /*label "Ln"*/ form ">9" at 2
            lv-delimiter
            rfqitem.part-no /*label "Customer Part#" */
            lv-delimiter
            rfqitem.stock-no /*label "FG Item#"*/
            lv-delimiter
            rfqitem.i-name form "x(30)"
            lv-delimiter
            rfqitem.style /*label "Style"*/ form "x(5)"
            lv-delimiter
            /*rfqitem.procat*/
            lv-len FORM ">>9.9999<"
            lv-delimiter
            lv-wid FORM ">>9.9999<"
            lv-delimiter
            lv-dep FORM ">>9.9999<"
            lv-delimiter
            rfqitem.i-col /*label "Ink" */ form ">>9"
            lv-delimiter
            rfqitem.i-coat /*label "Coat"*/ form ">>>9"
            lv-delimiter
            ls-est-no
            "   " lv-last-order-no SKIP.

   disp  "Quantity:" at 7 
         rfqitem.qty[1] no-label when rfqitem.qty[1] <> 0
         rfqitem.qty[2] no-label when rfqitem.qty[2] <> 0
         rfqitem.qty[3] no-label when rfqitem.qty[3] <> 0         
         rfqitem.qty[4] no-label when rfqitem.qty[4] <> 0         
         rfqitem.qty[5] no-label when rfqitem.qty[5] <> 0         
         rfqitem.qty[6] no-label when rfqitem.qty[6] <> 0         
         rfqitem.qty[7] no-label when rfqitem.qty[7] <> 0         
         rfqitem.qty[8] no-label when rfqitem.qty[8] <> 0         
         rfqitem.qty[9] no-label when rfqitem.qty[9] <> 0         
         rfqitem.qty[10] no-label when rfqitem.qty[10] <> 0 skip
         "Sell Price:" at 5
         rfqitem.qty-price[1] form "->>,>>9.99" when rfqitem.qty-price[1] <> 0
         rfqitem.qty-price[2] form "->>,>>9.99" when rfqitem.qty-price[2] <> 0
         rfqitem.qty-price[3] form "->>,>>9.99" when rfqitem.qty-price[3] <> 0
         rfqitem.qty-price[4] form "->>,>>9.99" when rfqitem.qty-price[4] <> 0
         rfqitem.qty-price[5] form "->>,>>9.99" when rfqitem.qty-price[5] <> 0
         rfqitem.qty-price[6] form "->>,>>9.99" when rfqitem.qty-price[6] <> 0
         rfqitem.qty-price[7] form "->>,>>9.99" when rfqitem.qty-price[7] <> 0
         rfqitem.qty-price[8] form "->>,>>9.99" when rfqitem.qty-price[8] <> 0
         rfqitem.qty-price[9] form "->>,>>9.99" when rfqitem.qty-price[9] <> 0
         rfqitem.qty-price[10] form "->>,>>9.99" when rfqitem.qty-price[10] <> 0
         skip
      /*   "__________" "__________" "__________" "__________"
         "__________" "__________" "__________" "__________" "__________" "__________"
      */
         "----------" at 17 "----------" "----------" "----------" "----------"
         "----------" "----------" "----------" "----------" "----------" 
         skip(2)
         with frame qty no-box no-label stream-io width 150.

     find style where style.company = rfqitem.company and
                    style.style = rfqitem.style
                    no-lock no-error.
     if avail style and style.industry = "1" /* folding carton */       
     then assign ls-label3 = "Stamp     :"
                 ls-label4 = "Laminate  :".
     else assign ls-label3 = "Joint Mat.:"
                 ls-label4 = "           ".

     disp
/*        fill("-",136) form "x(136)" at 2    */
          "---------------------------------------------  Miscellaneous Materials --------------------------------------------------"  at 10 skip
          "Board     :" at 10  rfqitem.board  rfqitem.brd-dscr  "    Caliper:" rfqitem.cal rfqitem.spec-no[1]  at 90 "Plate Cost  : _______________" skip
          "Windows   :" at 10  rfqitem.leaf[1] rfqitem.leaf-dscr[1] rfqitem.spec-no[2] at 90  "Die Cost    : _______________" skip
          "Foil      :" at 10  rfqitem.leaf[2] rfqitem.leaf-dscr[2] rfqitem.spec-no[3] at 90  "Artwork Cost: _______________" skip
          ls-label3 at 10  rfqitem.leaf[3] rfqitem.leaf-dscr[3] rfqitem.spec-no[4] at 90  "Priced By   : _______________" skip
          ls-label4 at 10  rfqitem.leaf[4] rfqitem.leaf-dscr[4] rfqitem.spec-no[5] at 90  "Date        : _______________" skip(1)
          "---------------------------------------------  Colors & Coating ---------------------------------------------"  at 10 skip
          rfqitem.i-ps[1] at 10 rfqitem.i-code[1]  rfqitem.i-dscr[1] rfqitem.i-ps[6] rfqitem.i-code[6] rfqitem.i-dscr[6] skip
          rfqitem.i-ps[2] at 10 rfqitem.i-code[2]  rfqitem.i-dscr[2] rfqitem.i-ps[7] rfqitem.i-code[7] rfqitem.i-dscr[7] skip
          rfqitem.i-ps[3] at 10 rfqitem.i-code[3]  rfqitem.i-dscr[3] rfqitem.i-ps[8] rfqitem.i-code[8] rfqitem.i-dscr[8] skip
          rfqitem.i-ps[4] at 10 rfqitem.i-code[4]  rfqitem.i-dscr[4] rfqitem.i-ps[9] rfqitem.i-code[9] rfqitem.i-dscr[9] skip
          rfqitem.i-ps[5] at 10 rfqitem.i-code[5]  rfqitem.i-dscr[5] rfqitem.i-ps[10] rfqitem.i-code[10] rfqitem.i-dscr[10] skip
         with frame det no-box no-label stream-io width 150.

    li-ln-cnt = li-ln-cnt + 1.
    if li-ln-cnt >= 2 then do:  /* display 2 line per page */
       page.
       view frame rfq-hd.
       li-ln-cnt = 0.
    end.

    if last-of(rfq.rfq-no) then do:

       disp 
            fill("-",136) form "x(136)" at 2 skip
            "Notes:" at 2 /*substring(rfq.inst,1,80) at 7 form "x(80)" skip
            substring(rfq.inst,1,80) at 7 form "x(80)" skip
            substring(rfq.inst,81,80) at 7 form "x(80)" skip
            substring(rfq.inst,161,80) at 7 form "x(80)" skip
            substring(rfq.inst,241,80) at 7 form "x(80)" skip
            substring(rfq.inst,321,80) at 7 form "x(80)" skip
            */
            rfq.inst view-as editor size 80 by 5 no-label
            fill("-",136) form "x(136)" at 2 
            with frame inst no-label no-box width 137 stream-io.
            .
       if not last(rfq.rfq-no) then page.
    end.  
END.  
put "***** End of Report *****" at 2 .
output close.


RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

