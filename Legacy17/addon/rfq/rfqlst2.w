&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: addon\rfq\rfqlst2.w

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


DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR ll-secure AS LOG NO-UNDO.
{sys/inc/VAR.i "new shared"}

def var k_frac as dec init 6.25 no-undo.

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
begin_cust-no end_cust-no rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_rfq-no end_rfq-no begin_cust-no ~
end_cust-no rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
td-show-parm 

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
     SIZE 96 BY 12.14.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 95 BY 5.48.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_rfq-no AT ROW 2.91 COL 25 COLON-ALIGNED
     end_rfq-no AT ROW 2.91 COL 58 COLON-ALIGNED
     begin_cust-no AT ROW 4.1 COL 25 COLON-ALIGNED
     end_cust-no AT ROW 4.1 COL 58 COLON-ALIGNED
     rd-dest AT ROW 10.05 COL 9 NO-LABEL
     lv-ornt AT ROW 10.05 COL 37 NO-LABEL
     lines-per-page AT ROW 10.05 COL 90 COLON-ALIGNED
     lv-font-no AT ROW 13.14 COL 40 COLON-ALIGNED
     lv-font-name AT ROW 14.1 COL 34 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.48 COL 40
     btn-ok AT ROW 21.71 COL 30
     btn-cancel AT ROW 21.71 COL 62
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 9.1 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 8
          BGCOLOR 2 
     RECT-6 AT ROW 8.86 COL 3
     RECT-7 AT ROW 1.48 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 99.2 BY 22.57.


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
         HEIGHT             = 22.81
         WIDTH              = 99.8
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  assign rd-dest begin_cust-no END_cust-no begin_rfq-no END_rfq-no.

  run run-report. 

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

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  find first rfq no-lock no-error.
  if avail rfq then begin_rfq-no = rfq.rfq-no.
  find last rfq no-lock no-error.
  if avail rfq then end_rfq-no = rfq.rfq-no.
  end_cust-no = "zzzz".

  RUN enable_UI.

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO begin_rfq-no.
  END.
  {methods/nowait.i}
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
  DISPLAY begin_rfq-no end_rfq-no begin_cust-no end_cust-no rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_rfq-no end_rfq-no begin_cust-no end_cust-no 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm btn-ok 
         btn-cancel 
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

     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 2, INPUT 0, INPUT 0, OUTPUT result).

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
  DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.

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
     rfqiso#-char FORM "x(20)" TO 112
     "Page:" + string(page-num,">>9") form "x(10)" to 126 skip
     fill("=",126)  form "x(126)"
     with frame rfq-title width 132 no-box no-label stream-io.
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
       /*find cust where cust.company = rfq.company and
                       cust.cust-no = rfq.cust-no
                       no-lock no-error. 
       assign lv-addr1 = if avail cust and rfq.cust-no <> "TEMP"
                         then cust.addr[1] else rfq.ship-addr[1]
              lv-addr2 = if avail cust and rfq.cust-no <> "TEMP" then cust.addr[2] else rfq.ship-addr[2]
              lv-city = if avail cust and rfq.cust-no <> "TEMP" then (cust.city + " " + cust.state + ", " + cust.zip)
                        else (rfq.ship-city + " " + rfq.ship-state + ", " + rfq.ship-zip) 
              lv-tel = if avail cust then cust.phone else "".
       if lv-addr2 = "" then assign lv-addr2 = lv-city
                                    lv-city = lv-tel.
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
                 fill("=",126) form "x(126)"
                  with frame rfq-hd no-labels width 132 no-box stream-io.
      */
        disp "RFQ#:" rfq.rfq-no form ">>>>>9"
             "  Customer:" rfq.cust-no rfq.ship-name form "x(30)"
             "     Req-Date: " rfq.req-date
             with frame hd no-box no-labels width 126. 
     PUT "Ln Customer Part#  FG Item#        Item Name                             Qty Style Category   Length    Width    Depth Ink Coat"
         SKIP
         "-- --------------- --------------- ------------------------------ ---------- ----- -------- -------- -------- -------- --- ----"
         SKIP.

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
          rfqitem.qty[1]
          lv-delimiter
          rfqitem.style FORM "x(5)"
          lv-delimiter
          rfqitem.procat FORM "x(8)"
          lv-delimiter
          lv-len FORM ">>>>9.99"
          lv-delimiter
          lv-wid FORM ">>>>9.99"
          lv-delimiter
          lv-dep FORM ">>>>9.99"
          lv-delimiter
          rfqitem.i-col /*label "Ink" */ FORM ">>9"
          lv-delimiter
          rfqitem.i-coat /*label "Coat"*/ FORM ">>>9"
          skip
          .
   ELSE PUT rfqitem.seq /*label "Ln"*/ form ">9"         
            lv-delimiter
            rfqitem.part-no /*label "Customer Part#" */
            lv-delimiter
            rfqitem.stock-no /*label "FG Item#"*/
            lv-delimiter
            rfqitem.i-name form "x(30)"
            lv-delimiter
            rfqitem.qty[1]
            lv-delimiter
            rfqitem.style    FORM "x(5)"
            lv-delimiter
            rfqitem.procat  FORM "x(8)"
            lv-delimiter
            lv-len FORM ">>9.9999"
            lv-delimiter
            lv-wid FORM ">>9.9999"
            lv-delimiter
            lv-dep FORM ">>9.9999"
            lv-delimiter
            rfqitem.i-col /*label "Ink" */    FORM ">>9"
            lv-delimiter
            rfqitem.i-coat /*label "Coat"*/   FORM ">>>9"
            SKIP
            .


/*
    if last-of(rfq.rfq-no) then do:
       disp       fill("-",125) form "x(125)" at 2 skip
            "Notes:" at 2 /*substring(rfq.inst,1,80) at 7 form "x(80)" skip
            substring(rfq.inst,1,80) at 7 form "x(80)" skip
            substring(rfq.inst,81,80) at 7 form "x(80)" skip
            substring(rfq.inst,161,80) at 7 form "x(80)" skip
            substring(rfq.inst,241,80) at 7 form "x(80)" skip
            substring(rfq.inst,321,80) at 7 form "x(80)" skip
            */
            rfq.inst view-as editor size 80 by 5 no-label
            fill("-",125) form "x(125)" at 2 
            with frame inst no-label no-box width 132 stream-io.
            .
       if not last(rfq.rfq-no) then page.
    end.  
  */  
END.  
put "***** End of Report *****" at 2 .
output close.



RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

