&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: cerep\r-estmar.w

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
&GLOBAL-DEFINE summary-sheet 1
DEFINE VARIABLE list-name AS cha NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/defines/globdefs.i}

DEFINE BUFFER b-prgrms FOR prgrms.

DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE Audit_File AS CHARACTER NO-UNDO.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.
DEFINE VARIABLE num-groups AS INTEGER NO-UNDO.
DEFINE VARIABLE group-ok   AS LOGICAL NO-UNDO.
DEFINE VARIABLE access-close AS LOGICAL NO-UNDO.

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
v-prgmname = USERID("NOSWEAT") + "..".
ELSE
ASSIGN
  v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1)
  v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
 cocode = gcompany
 locode = gloc.

{ce/msfcalc.i}
{ce/print4.i "new shared" "new shared"}


DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE qty AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-summ AS LOG INIT NO NO-UNDO.
DEFINE NEW SHARED VARIABLE fr-tot-pre AS DECIMAL.
DEFINE NEW SHARED VARIABLE gEstSummaryOnly AS LOG NO-UNDO.
DEFINE NEW SHARED BUFFER xest FOR est.
DEFINE NEW SHARED BUFFER xef FOR ef.
DEFINE NEW SHARED BUFFER xeb FOR eb.
DEFINE NEW SHARED BUFFER xop FOR est-op.

DEFINE BUFFER bf-probe FOR probe .
DEFINE STREAM st-excel.

DEFINE NEW SHARED VARIABLE chExcelApplication   AS COMPONENT-HANDLE                                             NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorkBook           AS COMPONENT-HANDLE                                                   NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorksheet          AS COMPONENT-HANDLE                                                   NO-UNDO.
DEFINE NEW SHARED VARIABLE chHyper              AS COMPONENT-HANDLE                                                   NO-UNDO. 
DEFINE VARIABLE v-cell                          AS CHARACTER                                                    NO-UNDO.
DEFINE VARIABLE t-dwg                           AS CHARACTER                                                         NO-UNDO.
DEFINE VARIABLE t-name                          AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE t-fnd                           AS LOGICAL  INIT "False"    NO-UNDO.
DEFINE VARIABLE t-seq                           AS INTEGER                                                              NO-UNDO.
DEFINE VARIABLE inRowCount                      AS INTEGER                                                      NO-UNDO    INITIAL 1.
DEFINE VARIABLE chFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE LvLineCnt AS INTEGER NO-UNDO.
DEFINE VARIABLE CurrDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE LvCtr AS INTEGER NO-UNDO.
DEFINE VARIABLE v-dir AS CHARACTER FORMAT "X(80)" NO-UNDO.
DEFINE VARIABLE vcTemplateFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE row-count AS INTEGER INIT 6 NO-UNDO.

DEFINE VARIABLE lv-brd-l           like eb.len no-undo.
DEFINE VARIABLE lv-brd-w           like lv-brd-l no-undo.
DEFINE VARIABLE lv-brd-sq          as dec format ">>>>9.9<<<<" no-undo.
DEFINE VARIABLE lv-brd-sf          as dec format ">>>>>9.9<<"  no-undo.
DEFINE VARIABLE lv-brd-wu          like lv-brd-sq no-undo.
DEF BUFFER bf-est FOR est.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 begin_cust-no end_cust-no begin_slsmn ~
end_slsmn begin_est end_est begin_date end_date begin_date-2 end_date-2 ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_slsmn ~
end_slsmn begin_est end_est begin_date end_date begin_date-2 end_date-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Beginning Add Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date-2 AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Beginning Mod Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_est AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     LABEL "Ending Add Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date-2 AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     LABEL "Ending Mod Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_est AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust-no AT ROW 3.14 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 3.14 COL 69 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_slsmn AT ROW 4.1 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 4.1 COL 69 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     begin_est AT ROW 5.05 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Estimate"
     end_est AT ROW 5.05 COL 69 COLON-ALIGNED HELP
          "Enter Ending Estimate"
     begin_date AT ROW 6 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 6 COL 69 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_date-2 AT ROW 6.95 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date-2 AT ROW 6.95 COL 69 COLON-ALIGNED HELP
          "Enter Ending Date"
     btn-ok AT ROW 10.29 COL 26
     btn-cancel AT ROW 10.29 COL 56
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 94.4 BY 12.29.


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
         TITLE              = "Customer Margin Analysis"
         HEIGHT             = 13.1
         WIDTH              = 95.6
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
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
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       begin_date-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       begin_est:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       end_date-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       end_est:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Customer Margin Analysis */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Customer Margin Analysis */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Add Date */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date-2 C-Win
ON LEAVE OF begin_date-2 IN FRAME FRAME-A /* Beginning Mod Date */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est C-Win
ON HELP OF begin_est IN FRAME FRAME-A /* Beginning Estimate# */
DO:
     DEFINE VARIABLE char-val AS cha NO-UNDO.
     DEFINE VARIABLE lv-eb-tmpid AS RECID NO-UNDO.

     RUN windows/l-est.w (g_company,g_loc,FOCUS:SCREEN-VALUE, OUTPUT char-val).

     IF char-val <> "" THEN DO:                 
            FIND FIRST eb WHERE STRING(RECID(eb)) = (char-val) NO-LOCK NO-ERROR.
            IF AVAILABLE eb THEN ASSIGN FOCUS:SCREEN-VALUE = eb.est-no
                                           lv-eb-tmpid = RECID(eb)    
                                begin_est:SCREEN-VALUE = eb.est-no.

            END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est C-Win
ON LEAVE OF begin_est IN FRAME FRAME-A /* Beginning Estimate# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning Sales Rep# */
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
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  RUN run-report.
  STATUS DEFAULT "Processing Complete".

   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Add Date */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date-2 C-Win
ON LEAVE OF end_date-2 IN FRAME FRAME-A /* Ending Mod Date */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est C-Win
ON HELP OF end_est IN FRAME FRAME-A /* Ending Estimate# */
DO:
     DEFINE VARIABLE char-val AS cha NO-UNDO.
     DEFINE VARIABLE lv-eb-tmpid AS RECID NO-UNDO.

     RUN windows/l-est.w (g_company,g_loc,FOCUS:SCREEN-VALUE, OUTPUT char-val).

     IF char-val <> "" THEN DO:                 
            FIND FIRST eb WHERE STRING(RECID(eb)) = (char-val) NO-LOCK NO-ERROR.
            IF AVAILABLE eb THEN ASSIGN FOCUS:SCREEN-VALUE = eb.est-no
                                           lv-eb-tmpid = RECID(eb)    
                                  end_est:SCREEN-VALUE = eb.est-no.

            END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est C-Win
ON LEAVE OF end_est IN FRAME FRAME-A /* Ending Estimate# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending Sales Rep# */
DO:
     ASSIGN {&self-name}.
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

/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_cust-no.
  END.

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
  DISPLAY begin_cust-no end_cust-no begin_slsmn end_slsmn begin_est end_est 
          begin_date end_date begin_date-2 end_date-2 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-7 begin_cust-no end_cust-no begin_slsmn end_slsmn begin_est 
         end_est begin_date end_date begin_date-2 end_date-2 btn-ok btn-cancel 
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
{custom/out2file.i}

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
/*RUN custom/d-print.w (fi_file).*/

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
 /*    DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 1, INPUT 0, INPUT 0, OUTPUT result).
                          /* font #*/ /* use-dialog(1) and landscape(2) */
                          */
 /*RUN custom/prntproc.p (fi_file,int(lv-font-no),lv-ornt).*/
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
  /*run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
{sys/form/r-topw.f}

DEFINE VARIABLE fest LIKE est.est-no NO-UNDO.
DEFINE VARIABLE test LIKE fest       NO-UNDO.

DEFINE VARIABLE li AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-box-size LIKE quoteitm.size NO-UNDO.
DEFINE VARIABLE lv-die-size LIKE quoteitm.size NO-UNDO.
DEFINE VARIABLE lv-format AS CHARACTER NO-UNDO.
DEFINE VARIABLE li-colors AS INTEGER NO-UNDO.
DEFINE VARIABLE li-qty LIKE probe.est-qty NO-UNDO.
DEFINE VARIABLE ld-costm LIKE probe.full-cost NO-UNDO.
DEFINE VARIABLE ld-costt AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE ld-price LIKE probe.sell-price NO-UNDO.
DEFINE VARIABLE ld-mar AS DECIMAL FORMAT "->>,>>>,>>9.99" EXTENT 7 NO-UNDO.
DEFINE VARIABLE ld-pct AS DECIMAL NO-UNDO.
DEFINE VARIABLE k_frac AS DECIMAL INIT "6.25" NO-UNDO.
DEFINE VARIABLE v-tons AS DECIMAL NO-UNDO .
DEFINE VARIABLE qm AS DECIMAL.
DEFINE VARIABLE lv-eqty LIKE est-op.qty NO-UNDO.
DEFINE VARIABLE v-brd-cost AS DECIMAL NO-UNDO.
DEFINE VARIABLE CALL_id AS RECID NO-UNDO.
DEFINE VARIABLE dTonCost AS DECIMAL NO-UNDO .
DEFINE VARIABLE iAvg AS INTEGER NO-UNDO INIT 0 .
DEFINE BUFFER reftable-fm FOR reftable.
DEFINE BUFFER probe-ref FOR reftable.
DEFINE BUFFER probe-fm FOR reftable.
ASSIGN row-count = 6 .

/**************************** Excel Initilization Starts *********************************/

FIND FIRST users WHERE
     users.user_id EQ USERID("NOSWEAT")
     NO-LOCK NO-ERROR.

IF AVAILABLE users AND users.user_program[2] NE "" THEN
   v-dir = users.user_program[2] + "\".
ELSE
   v-dir = "c:\tmp\".

/* Connect to the running Excel session. */
  CREATE "Excel.Application" chExcelApplication.

  FILE-INFO:FILE-NAME = "template\CustMarginAnalysis.xlt". /* CustMarginAnalysis*/

  /* Set the Excel Template to be used. */
  ASSIGN chFile = SEARCH (FILE-INFO:FULL-PATHNAME) no-error.
  
  IF SEARCH (chFile) = ? THEN DO:
    MESSAGE 'Spreadsheet File: ' FILE-INFO:FULL-PATHNAME
            'cannot be found. Please verify that the file exists.'
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    APPLY 'CLOSE':U TO THIS-PROCEDURE.
  END.

  /* Make Excel visible. */
  ASSIGN
     chFile = FILE-INFO:FULL-PATHNAME.


 /* Open our Excel Template. */  
  ASSIGN chWorkbook = chExcelApplication:Workbooks:Open(chfile)  no-error.

  chExcelApplication:VISIBLE = TRUE.
  
  /* Do not display Excel error messages. */
  chExcelApplication:DisplayAlerts = FALSE  NO-ERROR.
  
  /* Disable screen updating so it will go faster */
  chExcelApplication:ScreenUpdating = FALSE.

  chWorkbook:WorkSheets({&summary-sheet}):Activate NO-ERROR.
   ASSIGN
     chWorkSheet = chExcelApplication:Sheets:item({&summary-sheet})  .
/**************************** Excel Initilization End *********************************/


ASSIGN
 str-tit2 = TRIM(c-win:TITLE) + ""
 {sys/inc/ctrtext.i str-tit2 112}

 fest = FILL(" ",8 - LENGTH(TRIM(begin_est))) + TRIM(begin_est)
 test = FILL(" ",8 - LENGTH(TRIM(end_est))) + TRIM(end_est).

{sys/inc/print1.i}

{sys/inc/outprint.i  VALUE(74)}

SESSION:SET-WAIT-STATE ("general").

/*IF td-show-parm THEN RUN show-param.*/

VIEW FRAME r-top.
MAIN-LOOP:
FOR EACH est NO-LOCK
    WHERE est.company  EQ cocode
      AND est.est-no   GE fest
      AND est.est-no   LE test
      AND est.est-date GE begin_date
      AND est.est-date LE end_date
      AND est.mod-date GE begin_date-2
      AND est.mod-date LE end_date-2
      AND (est.est-type EQ 4 OR est.est-type EQ 8)
    ,

    FIRST est-qty NO-LOCK
    WHERE est-qty.company EQ est.company
      AND est-qty.est-no  EQ est.est-no
    ,

    EACH eb NO-LOCK
    WHERE eb.company  EQ est.company
      AND eb.est-no   EQ est.est-no
      AND eb.cust-no  GE begin_cust-no
      AND eb.cust-no  LE end_cust-no
      AND eb.sman     GE begin_slsmn
      AND eb.sman     LE end_slsmn
     /* AND (eb.form-no EQ 0 OR (eb.est-type NE 2 AND eb.est-type NE 6))*/
    ,

    FIRST ef NO-LOCK
    WHERE ef.company EQ eb.company
      AND ef.est-no  EQ eb.est-no
      AND ef.form-no EQ eb.form-no
    ,

    EACH probe NO-LOCK
    WHERE probe.company   EQ est.company
      AND probe.est-no    EQ est.est-no
      AND probe.full-cost NE ?

    BREAK BY est.est-no DESCENDING
          BY probe.est-qty
          BY probe.probe-date
          BY probe.probe-time:

    {custom/statusMsg.i " 'Processing Estimate#:  '  + eb.est-no  "}
        FIND FIRST job-hdr NO-LOCK 
             WHERE job-hdr.company EQ cocode
               AND job-hdr.est-no EQ est.est-no NO-ERROR .
    
    IF NOT AVAILABLE job-hdr THEN  NEXT MAIN-LOOP .

    IF LAST-OF(probe.est-qty) THEN DO:

    FOR EACH kli:
        DELETE kli.
    END.

    FOR EACH ink:
        DELETE ink.
    END.

    FOR EACH flm:
        DELETE flm.
    END.

    FOR EACH cas:
        DELETE cas.
    END.
    
    FOR EACH car:
        DELETE car.
    END.
    
    FOR EACH brd:
        DELETE brd.
    END.

    FOR EACH blk:
        DELETE blk.
    END.

    FOR EACH xjob:
        DELETE xjob.
    END.
    FIND xest NO-LOCK WHERE RECID(xest) = recid(est)  NO-ERROR.
    FIND xef  NO-LOCK WHERE RECID(xef) = recid(ef)  NO-ERROR.
    FIND xeb  NO-LOCK WHERE RECID(xeb) = recid(eb)  NO-ERROR.

   save-lock = xef.op-lock.
   {est/recalc-mr.i xest}
  FIND CURRENT recalc-mr NO-LOCK.

  ASSIGN
   do-speed = xest.recalc
   do-mr    = recalc-mr.val[1] EQ 1
   do-gsa   = xest.override.

  {sys/inc/cerun.i F}
  vmclean = LOOKUP(cerunf,"McLean,HOP") GT 0.

  {ce/msfcalc.i}

   DO TRANSACTION:
  {est/op-lock.i xest}
  FIND bf-est WHERE RECID(bf-est) EQ RECID(xest).
  FIND CURRENT recalc-mr.
  ASSIGN
   bf-est.recalc    = do-speed
   recalc-mr.val[1] = INT(do-mr)
   bf-est.override  = do-gsa
   op-lock.val[1]   = INT(bf-est.recalc)
   op-lock.val[2]   = recalc-mr.val[1].
  FIND CURRENT bf-est NO-LOCK.
  FIND CURRENT recalc-mr NO-LOCK.
  FIND CURRENT op-lock NO-LOCK.
  /*FIND xest WHERE RECID(xest) EQ RECID(bf-est).   */
  END.
    
    li-colors =  0.
    op-tot    =  0.
    v-tons    =  0.
    
    RELEASE probeit.
    IF est.est-type EQ 3 OR est.est-type EQ 4 OR
       est.est-type EQ 7 OR est.est-type EQ 8 THEN
      FIND FIRST probeit NO-LOCK
          WHERE probeit.company EQ probe.company
            AND probeit.est-no  EQ probe.est-no
            AND probeit.line    EQ probe.line
            AND probeit.part-no EQ eb.part-no
          NO-ERROR.

    IF AVAILABLE probeit THEN
      ASSIGN
       li-qty   = IF probeit.yrprice THEN probeit.yld-qty ELSE probeit.bl-qty
       ld-costm = probeit.full-cost
       ld-price = probeit.sell-price.
    ELSE
      ASSIGN
       li-qty   = probe.est-qty
       ld-costm = probe.full-cost
       ld-price = probe.sell-price.

    ld-costt = li-qty / 1000 * ld-costm.

    ld-pct = .85.
    DO li = 1 TO EXTENT(ld-mar):
      ASSIGN
       ld-mar[li] = (ld-costt / ld-pct * 1.01) - ld-costt
       ld-pct     = ld-pct - .05.
    END.

    DO TRANSACTION:
        FOR EACH est-op
            WHERE est-op.company EQ xest.company 
            AND est-op.est-no  EQ xest.est-no
            AND est-op.line    GT 500:
            DELETE est-op.
         END.
         FOR EACH est-op
             WHERE est-op.company EQ xest.company
             AND est-op.est-no  EQ xest.est-no
             AND est-op.line    LT 500
             BY est-op.qty:
             lv-eqty = est-op.qty.
             LEAVE.
          END.
          FOR EACH est-op
              WHERE est-op.company EQ xest.company 
              AND est-op.est-no  EQ xest.est-no
              AND est-op.qty     EQ lv-eqty
              AND est-op.line    LT 500:
              CREATE xop.
              BUFFER-COPY est-op EXCEPT rec_key TO xop.
              xop.line = est-op.line + 500.
          END.
      END.


    /* run ce/com/prokalk.p.*/
    FOR EACH xef WHERE xef.company = xest.company
                   AND xef.est-no EQ xest.est-no:
    
       xxx = 0.
       FOR EACH xeb WHERE xeb.company = xest.company
                   AND xeb.est-no EQ xest.est-no AND xeb.form-no = xef.form-no
           BY xeb.blank-no:
          FIND FIRST kli WHERE kli.cust-no = xeb.cust-no NO-ERROR.
          IF NOT AVAILABLE kli THEN DO:
             FIND FIRST sman   WHERE sman.company = cocode AND  
                                     sman.sman    = xeb.sman NO-LOCK NO-ERROR.
             FIND FIRST cust   WHERE   cust.company = cocode AND
                                       cust.cust-no = xeb.cust-no NO-LOCK NO-ERROR.
             FIND FIRST shipto WHERE shipto.company = cust.company AND
                                     shipto.cust-no = cust.cust-no AND
                                     shipto.ship-id = xeb.ship-id NO-LOCK NO-ERROR.
             CREATE kli.
             IF AVAILABLE sman THEN ASSIGN kli.sman    = sman.sman
                                         kli.sname   = sman.sname.
             IF xeb.cust-no NE "Temp" THEN ASSIGN
             kli.cust-no = xeb.cust-no
             kli.cust-add[1] = cust.name
             kli.cust-add[2] = cust.addr[1]
             kli.cust-add[3] = cust.addr[2]
             kli.cust-add[4] = cust.city + ", " + cust.state + " " + cust.zip.
             ELSE ASSIGN
             kli.cust-no = xeb.cust-no
             kli.cust-add[1] = xeb.ship-name
             kli.cust-add[2] = xeb.ship-addr[1]
             kli.cust-add[3] = xeb.ship-addr[2]
             kli.cust-add[4] = xeb.ship-city + ", " + xeb.ship-state + " " +
                               xeb.ship-zip.
    
             IF kli.cust-add[3] = "" THEN ASSIGN
                kli.cust-add[3] = kli.cust-add[4] kli.cust-add[4] = "".
             IF kli.cust-add[2] = "" THEN ASSIGN
                kli.cust-add[2] = kli.cust-add[3] kli.cust-add[3] = kli.cust-add[4]
                kli.cust-add[4] = "".
             ASSIGN
             kli.ship-add[1] = shipto.ship-name
             kli.ship-add[2] = shipto.ship-addr[1]
             kli.ship-add[3] = shipto.ship-addr[2]
             kli.ship-add[4] = shipto.ship-city + ", " + shipto.ship-state +
                                                             " " + shipto.ship-zip.
             IF kli.ship-add[3] = "" THEN
             ASSIGN kli.ship-add[3] = kli.ship-add[4] kli.ship-add[4] = "".
             IF kli.ship-add[2] = "" THEN
             ASSIGN kli.ship-add[2] = kli.ship-add[3]
                    kli.ship-add[3] = kli.ship-add[4] kli.ship-add[4] = "".
          END.
          FIND FIRST blk WHERE blk.snum = xeb.form-no AND
                               blk.bnum = xeb.blank-no NO-ERROR.
          IF NOT AVAILABLE blk THEN DO:
             CREATE blk.
             ASSIGN
              blk.kli      = kli.cust-no
              blk.id       = xeb.part-no
              blk.snum     = xeb.form-no
              blk.bnum     = xeb.blank-no
              blk.qreq     = xeb.bl-qty
              blk.qyld     = xeb.yld-qty
              blk.yr$      = xeb.yrprice
              blk.stock-no = xeb.stock-no.
          END.
          xxx = xxx + (xeb.t-sqin * xeb.num-up).
       END.
       FOR EACH xeb WHERE xeb.company = xest.company
                      AND xeb.est-no EQ xest.est-no
                      AND xeb.form-no EQ xef.form-no NO-LOCK,
           FIRST blk  WHERE blk.snum EQ xeb.form-no
                        AND blk.bnum EQ xeb.blank-no:
           blk.pct = (xeb.t-sqin * xeb.num-up) / xxx.
       END.
    END.
    

    ASSIGN t-blksht = 0
           tt-blk   = 0
           t-blkqty = 0
           vbsf     = 0 
           dTonCost = 0 
           iAvg     = 0.
    FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK NO-ERROR.
    ASSIGN
        ctrl[1]  = ce-ctrl.whse-mrkup / 100
        ctrl[2]  = ce-ctrl.hand-pct / 100
        ctrl[3]  = ce-ctrl.rm-rate
        ctrl[4]  = ce-ctrl.spec-%[1]
        ctrl[5]  = int(ce-ctrl.comm-add)
        ctrl[6]  = int(ce-ctrl.shp-add)
        ctrl[7]  = int(ce-ctrl.sho-labor)
        ctrl[8]  = int(ce-ctrl.trunc-99)
        ctrl[11] = ce-ctrl.spec-%[2]
        ctrl[12] = ce-ctrl.spec-%[3]
        ctrl[13] = int(ce-ctrl.spec-add[1])
        ctrl[14] = int(ce-ctrl.spec-add[2])
        ctrl[15] = int(ce-ctrl.spec-add[3])
        ctrl[16] = int(ce-ctrl.spec-add[6])
        ctrl[17] = int(ce-ctrl.spec-add[7])
        ctrl[18] = int(ce-ctrl.spec-add[8]).

    FOR EACH xef WHERE xef.company = xest.company
               AND xef.est-no EQ xest.est-no
               BREAK BY xef.form-no :
    

    FOR EACH xeb OF xef BY xeb.blank-no:

      FIND FIRST item {sys/look/itemW.i} AND item.i-no = xef.board NO-LOCK NO-ERROR.
      IF AVAILABLE item THEN FIND FIRST e-item OF item NO-LOCK NO-ERROR.
      /* set total # of blanks on all forms */

      ASSIGN
      tt-blk = tt-blk + IF xeb.yrprice /*AND NOT ll-tandem*/ THEN xeb.yld-qty ELSE xeb.bl-qty
      /* set total # of blanks on this form */
      t-blksht[xef.form-no] = t-blksht[xef.form-no] + xeb.num-up
      /* set total qty of all blanks for this form */
      t-blkqty[xeb.form-no] = t-blkqty[xeb.form-no] +
                              IF xeb.yrprice THEN xeb.yld-qty ELSE xeb.bl-qty.
      /* find sheet qty needed for this form (without spoil)*/
      IF (xeb.yld-qty / xeb.num-up) > zzz THEN
      ASSIGN zzz = (xeb.yld-qty / xeb.num-up).
      {sys/inc/roundup.i zzz}
      ASSIGN
      t-shtfrm[xeb.form-no] = zzz
      call_id = RECID(xeb)
      vbsf = vbsf + IF v-corr THEN (xeb.t-sqin * .007) ELSE (xeb.t-sqin / 144)
      brd-l[4]  = xeb.t-len
      brd-w[4]  = xeb.t-wid
      brd-sq[4] = xeb.t-sqin  /*brd-l[4] * brd-w[4]*/
      brd-sf[4] = IF v-corr THEN (brd-sq[4] * .007) ELSE (brd-sq[4] / 144)
      brd-wu[4] = brd-sf[4] * item.basis-w.
     
   END.
   FIND xeb WHERE RECID(xeb) = call_id NO-LOCK NO-ERROR. qty = xeb.yld-qty.

   RUN ce/com/prokalk.p.

    v-tons = v-tons + (IF v-corr THEN (xef.gsh-len * xef.gsh-wid * .007)
                          ELSE (xef.gsh-len * xef.gsh-wid / 144) ) * xef.gsh-qty  / 1000 * ( IF AVAIL ITEM THEN item.basis-w ELSE 0) / 2000 .
 
   qty = IF eb.yrprice /*AND NOT ll-tandem*/ THEN eb.yld-qty ELSE eb.bl-qty.  
   
   iAvg = iAvg + 1.
   dm-tot[3] = 0. dm-tot[4] = 0. dm-tot[5] = 0.
    
   /* b o a r d        */  RUN ce/com/pr4-brd.p ("").
     
   /* i n k s          */ RUN ce/com/pr4-ink.p.
    
   /* film             */ RUN ce/com/pr4-flm.p.
      
   /* case/tray/pallet */ RUN ce/com/pr4-cas.p.
     
   /* special          */ RUN ce/com/pr4-spe.p.
       
      ASSIGN dTonCost = dTonCost + b-msh .
      
    END. /* for each xef */

    ASSIGN dTonCost = dTonCost / iAvg .

      qm = probe.est-qty / 1000.

      v-brd-cost = v-brd-cost + dm-tot[5].
      FOR EACH blk:
          FIND FIRST xjob
              WHERE xjob.i-no     EQ blk.id
              AND xjob.form-no  EQ blk.snum
              AND xjob.blank-no EQ blk.bnum
              NO-ERROR.

          IF NOT AVAILABLE xjob THEN DO:
              CREATE xjob.
              ASSIGN
                  xjob.form-no  = blk.snum
                  xjob.blank-no = blk.bnum
                  xjob.cust-no  = blk.kli.
          END.

          ASSIGN
              xjob.mat      = blk.cost - blk.lab
              xjob.lab      = blk.lab
              xjob.i-no     = blk.id
              xjob.pct      = blk.pct
              xjob.stock-no = blk.stock-no.
     END.
       
      /* prep      */ RUN ce/com/pr4-prp.p.
      
      /* machines */ RUN ce/com/pr4-mch.p.
    
      IF ctrl2[2] NE 0 OR ctrl2[3] NE 0 THEN DO:
          op-tot[5] = op-tot[5] + (ctrl2[2] + ctrl2[3]).
      END.
   
   
   /* mat */
   DO i = 1 TO 6:
      ctrl[9] = ce-ctrl.mat-pct[i] / 100.
      IF ce-ctrl.mat-cost[i] > dm-tot[5]  THEN LEAVE.
   END.

    /* lab */
   DO i = 1 TO 6:
      ctrl[10] = ce-ctrl.lab-pct[i] / 100.
      IF ce-ctrl.lab-cost[i] > op-tot[5]  THEN LEAVE.
   END.
   
  DO TRANSACTION:
     {est/calcpcts.i xest}
     ASSIGN
      calcpcts.val[1] = ctrl[9] * 100
      calcpcts.val[2] = v-brd-cost.
     FIND CURRENT calcpcts NO-LOCK NO-ERROR.
   END.
   
  ASSIGN
       gsa-mat = ctrl[9]  * 100
       gsa-lab = ctrl[10] * 100
       gsa-com = ce-ctrl.comm-mrkup
       gsa-war = ce-ctrl.whse-mrkup
        .

   FIND FIRST reftable-fm NO-LOCK
       WHERE reftable-fm.reftable EQ "gsa-fm"
       AND reftable-fm.company  EQ xest.company
       AND reftable-fm.loc      EQ ""
       AND reftable-fm.code     EQ xest.est-no
       NO-ERROR.

   IF AVAILABLE reftable-fm THEN
       gsa-fm = reftable-fm.val[1].
   ELSE
       gsa-fm = ctrl[19].

    
       ASSIGN
        gsa-mat = probe.gsa-mat
        gsa-lab = probe.gsa-lab 
        gsa-war = probe.gsa-war  .

       FIND FIRST probe-ref NO-LOCK
          WHERE probe-ref.reftable EQ "probe-ref"
            AND probe-ref.company  EQ probe.company
            AND probe-ref.loc      EQ ""
            AND probe-ref.code     EQ probe.est-no
            AND probe-ref.code2    EQ STRING(probe.line,"9999999999")
          NO-ERROR.
      IF AVAILABLE probe-ref THEN 
          DO TRANSACTION:
            FIND CURRENT calcpcts NO-ERROR.
            calcpcts.val[1] = probe-ref.val[1] .
            FIND CURRENT calcpcts NO-LOCK NO-ERROR.
          END.

          FIND FIRST probe-fm NO-LOCK
          WHERE probe-fm.reftable EQ "gsa-fm"
            AND probe-fm.company  EQ probe.company
            AND probe-fm.loc      EQ ""
            AND probe-fm.code     EQ probe.est-no
          NO-ERROR.

      IF AVAIL probe-fm THEN
         gsa-fm = probe-fm.val[1].
   
   ASSIGN
       ctrl[9]  = gsa-mat / 100 
       ctrl[10] = gsa-lab / 100 
       ctrl[1]  = gsa-war / 100
       ctrl[19] = gsa-fm / 100.
   
   RUN ce/com/pr4-tots.p.

   FIND FIRST item NO-LOCK
        WHERE ITEM.company EQ eb.company
          AND item.i-no = ef.board NO-ERROR.
   IF AVAILABLE item THEN FIND FIRST e-item OF item NO-LOCK NO-ERROR.

   IF ctrl2[9] EQ ? THEN ctrl2[9] = 0.
   IF ctrl2[10] EQ ? THEN ctrl2[10] = 0.
   IF v-tons EQ ? THEN v-tons = 0 .

  

    DISPLAY eb.cust-no            FORMAT "x(8)" COLUMN-LABEL "Customer"
            est.est-date           COLUMN-LABEL "date"
            TRIM(eb.est-no)       FORMAT "x(8)"
                                  COLUMN-LABEL "Est#"
            probe.sell-price      COLUMN-LABEL "Selling Price"
            v-tons                COLUMN-LABEL "Board Tons"
            ""                    COLUMN-LABEL "Board Cost / Ton (ERP)"
            ""                     COLUMN-LABEL "Board Cost / Ton (Actual)"
            ""                    COLUMN-LABEL  "Board Pad"
            calcpcts.val[2]        COLUMN-LABEL "GSA MU B"
            ctrl2[9]               COLUMN-LABEL "GSA MU M"
            ctrl2[10]              COLUMN-LABEL "GSA Labor"
            probe.full-cost * qm             COLUMN-LABEL "ERP Cost"
            ""                    COLUMN-LABEL "ERP Margin ($)"
            ""           COLUMN-LABEL "ERP Margin (%)"
            ""              COLUMN-LABEL "Est. Cost"
            ""           COLUMN-LABEL "Est. Margin ($)"
            ""            COLUMN-LABEL "Est. Margin (%)"
           

           WITH FRAME est DOWN NO-BOX STREAM-IO WIDTH 300.
        
    ASSIGN
          chWorkSheet:Range("C" + STRING(row-count)):VALUE = eb.cust-no .
          chWorkSheet:Range("D" + STRING(row-count)):VALUE = est.est-date .
          chWorkSheet:Range("E" + STRING(row-count)):VALUE = TRIM(eb.est-no) .
          chWorkSheet:Range("F" + STRING(row-count)):VALUE = STRING(probe.sell-price * qm, "->>>>>>>9.99") .
          chWorkSheet:Range("G" + STRING(row-count)):VALUE = STRING(v-tons, "->>9.99999") .
          chWorkSheet:Range("H" + STRING(row-count)):VALUE = STRING(dTonCost, "->>>>>>>9.99") .
          /*chWorkSheet:Range("I" + STRING(row-count)):VALUE = "" .*/
          /*chWorkSheet:Range("J" + STRING(row-count)):VALUE = "" .*/
          chWorkSheet:Range("K" + STRING(row-count)):VALUE =  calcpcts.val[2] .
          chWorkSheet:Range("L" + STRING(row-count)):VALUE = ctrl2[9] .
          chWorkSheet:Range("M" + STRING(row-count)):VALUE = ctrl2[10]  .
          chWorkSheet:Range("N" + STRING(row-count)):VALUE = ctrl2[1] .
          chWorkSheet:Range("O" + STRING(row-count)):VALUE = probe.full-cost * qm . 
          chWorkSheet:Range("P" + STRING(row-count)):VALUE = probe.sell-price * (probe.net-profit / 100) * qm .
          chWorkSheet:Range("Q" + STRING(row-count)):VALUE = string(probe.net-profit,"->>9.99%") .
          
           row-count = row-count + 1 .
    
      END.
END.

chWorkSheet:Range ("A" + string(row-count) + "..S1020" ):delete     NO-ERROR.

/*OUTPUT STREAM st-excel CLOSE.*/
/* RELEASE OBJECTS */

/* size the columns automatically */
  chWorkbook:WorkSheets({&summary-sheet}):Columns("D:I"):AutoFit.
  

  /* let the user in */
  ASSIGN chExcelApplication:VISIBLE = TRUE.
  chWorkbook:WorkSheets({&summary-sheet}):Activate NO-ERROR.

  ASSIGN
     chWorkSheet = chExcelApplication:Sheets:item({&summary-sheet})
     /* enable screen updating */
     chExcelApplication:ScreenUpdating = TRUE.


  RELEASE OBJECT chWorkbook         NO-ERROR.
  RELEASE OBJECT chWorkSheet        NO-ERROR.
  RELEASE OBJECT chExcelApplication NO-ERROR.
  RELEASE xeb .
  RELEASE xop .
  RELEASE eb.
  RELEASE ef.
  RELEASE probe .

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */

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

  lv-frame-hdl = FRAME {&frame-name}:handle.
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

