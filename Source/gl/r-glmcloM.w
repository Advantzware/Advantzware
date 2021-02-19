&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: gl/r-glmcloM.w

  Description: Cost Estimating Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: sewa singh

  Created: 01/18/2021

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
DEFINE INPUT PARAMETER ipcModule AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.
DEF VAR v-invalid AS LOG NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

def var save_id as recid.
def var time_stamp as ch.

def var start-date as date initial 01/01/1901 NO-UNDO.
def var end-date as date initial 01/01/1901 NO-UNDO.
def var tot-all  as dec format "->>>,>>>,>>>,>>9.99" NO-UNDO.
def var tot-tx   like tot-all NO-UNDO.
def var tot-act  like tot-all NO-UNDO.
def var tot-jrnl like tot-all NO-UNDO.
def var open-amt like tot-all NO-UNDO.
def var net-inc  as dec NO-UNDO.
def var per-open as inte format ">9" NO-UNDO.
def var per-status like period.pstat NO-UNDO.
def var fiscal-yr like period.yr NO-UNDO.

def buffer b-racct for account.
def buffer b-cacct for account.
DEF VAR uperiod AS INT NO-UNDO.
DEF VAR choice AS LOG NO-UNDO.
DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" NO-UNDO.
DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" NO-UNDO.
DEFINE VARIABLE cModuleTitle AS CHARACTER NO-UNDO.

ASSIGN time_stamp = string(time,"hh:mmam")
       .
DEF STREAM excel.
IF ipcModule EQ "AP" THEN
ASSIGN cModuleTitle = "Close A/P – Payables" .
ELSE IF ipcModule EQ "PO" THEN
ASSIGN cModuleTitle = "Close P/O – Purchasing" .
ELSE IF ipcModule EQ "OP" THEN
ASSIGN cModuleTitle = "Close O/P - Order Processing" .
ELSE IF ipcModule EQ "WIP" THEN
ASSIGN cModuleTitle = "Close WIP - Work In Process" .
ELSE IF ipcModule EQ "RM" THEN
ASSIGN cModuleTitle = "Close R/M - Inventory" .
ELSE IF ipcModule EQ "FG" THEN
ASSIGN cModuleTitle = "Close F/G –Inventory" .
ELSE IF ipcModule EQ "BR" THEN
ASSIGN cModuleTitle = "Close B/R - Bank Reconciliation" .
ELSE IF ipcModule EQ "AR" THEN
ASSIGN cModuleTitle = "Close A/R – Receivables" .  


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 tb_sub-report tb_close-sub btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fiText tran-year tran-period tb_sub-report ~
tb_close-sub 

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

DEFINE VARIABLE fiText AS CHARACTER FORMAT "X(300)":U INITIAL "This operation will perform a CLOSE on your first open period for this sub ledger" 
     VIEW-AS FILL-IN 
     SIZE 78 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE tran-year AS INTEGER FORMAT ">>>>":U INITIAL 0 
     LABEL "Year" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.81.

DEFINE VARIABLE tb_sub-report AS LOGICAL INITIAL no 
     LABEL "Sub Ledger Report" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_close-sub AS LOGICAL INITIAL no 
     LABEL "Close Sub Ledger" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fiText AT ROW 2.43 COL 9 COLON-ALIGNED NO-LABEL
     tran-year AT ROW 3.62 COL 20 COLON-ALIGNED WIDGET-ID 6
     tran-period AT ROW 3.62 COL 39 COLON-ALIGNED
     tb_sub-report AT ROW 5.50 COL 21.8 WIDGET-ID 10
     tb_close-sub AT ROW 7.10 COL 21.8 WIDGET-ID 12
     btn-ok AT ROW 10.29 COL 18
     btn-cancel AT ROW 10.29 COL 57
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 11.33.


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
         TITLE              = cModuleTitle
         HEIGHT             = 11.76
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

/* SETTINGS FOR FILL-IN fiText IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       fiText:READ-ONLY IN FRAME FRAME-A        = TRUE.

ASSIGN 
       tb_sub-report:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_close-sub:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tran-period:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-year IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tran-year:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Close A/P – Payables */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Close A/P – Payables */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
  ASSIGN 
       tran-period
       uperiod = tran-period
       .

  run check-date (YES).
  if v-invalid then return no-apply.       

  assign /*rd-dest*/
         
         tran-period
         uperiod = tran-period
         .
 /*IF tb_sub-report THEN
 DO:
  run run-report. 

  run output-to-screen.
 END.*/     
  IF tb_close-sub THEN DO:
     choice = NO.
     MESSAGE " Close " ipcModule " Period" uperiod VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
           UPDATE choice.
     IF choice THEN do:
        RUN close-month.
        MESSAGE "Closing " ipcModule " Period is completed. " VIEW-AS ALERT-BOX INFO.       

     END.
  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sub-report
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sub-report C-Win
ON VALUE-CHANGED OF tb_sub-report IN FRAME FRAME-A /* JE's with inactive account */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_close-sub
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_close-sub C-Win
ON VALUE-CHANGED OF tb_close-sub IN FRAME FRAME-A /* Out of balance entries */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-period C-Win
ON LEAVE OF tran-period IN FRAME FRAME-A /* Period */
DO:
  assign {&self-name}.
  if lastkey ne -1 then do:
    run check-date (NO).
    if v-invalid then DO:
        ASSIGN SELF:SCREEN-VALUE = "".
        APPLY 'entry' TO tran-year.
        return no-apply.
    END.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-year
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-year C-Win
ON LEAVE OF tran-year IN FRAME FRAME-A /* Year */
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

    IF access-close THEN DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.
  
    tran-year = YEAR(TODAY) .
    tran-period = (MONTH(TODAY))  .
  
    FIND company NO-LOCK WHERE 
        company.company EQ cocode
        NO-ERROR.
    IF NOT AVAIL company THEN DO:
        MESSAGE 
            "Company " + cocode + " does not exist in the company file."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    if NOT company.yend-per then do:
        MESSAGE 
            "PRIOR YEAR NOT CLOSED.  MUST CLOSE PRIOR YEAR!!!" 
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    end.
    FIND FIRST period NO-LOCK WHERE 
        period.company EQ company.company AND
        period.pstat EQ TRUE 
        NO-ERROR.
    IF AVAIL period THEN ASSIGN 
        tran-year = period.yr
        tran-period = period.pnum.
 
  RUN enable_UI.

    {methods/nowait.i}
    DO with frame {&frame-name}:
        APPLY "entry" TO tran-year.
    END.
    
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
  DEF INPUT PARAM ip-oktogo AS LOG NO-UNDO.

  def buffer alt-period for period.


  DO with frame {&frame-name}:
    v-invalid = no.

    find first period                   
        where period.company eq cocode
          AND period.yr   EQ tran-year
          AND period.pnum EQ tran-period           
        no-lock no-error.
    if avail period THEN DO:
       IF period.subLedgerAP EQ "C" AND ipcModule EQ "AP" THEN DO:
          MESSAGE "Payables - Already Closed. " VIEW-AS ALERT-BOX ERROR.
          v-invalid = YES.
       END.
       ELSE IF period.subLedgerPO EQ "C" AND ipcModule EQ "PO" THEN DO:
          MESSAGE "Purchasing - Already Closed. " VIEW-AS ALERT-BOX ERROR.
          v-invalid = YES.
       END.
       ELSE IF period.subLedgerOP EQ "C" AND ipcModule EQ "OP" THEN DO:
          MESSAGE "Order Processing - Already Closed. " VIEW-AS ALERT-BOX ERROR.
          v-invalid = YES.
       END.
       ELSE IF period.subLedgerWIP EQ "C" AND ipcModule EQ "WIP" THEN DO:
          MESSAGE "Work In Process - Already Closed. " VIEW-AS ALERT-BOX ERROR.
          v-invalid = YES.
       END.
       ELSE IF period.subLedgerRM EQ "C" AND ipcModule EQ "RM" THEN DO:
          MESSAGE "R/M Inventory - Already Closed. " VIEW-AS ALERT-BOX ERROR.
          v-invalid = YES.
       END.
       ELSE IF period.subLedgerFG EQ "C" AND ipcModule EQ "FG" THEN DO:
          MESSAGE "F/G Inventory - Already Closed. " VIEW-AS ALERT-BOX ERROR.
          v-invalid = YES.
       END.
       ELSE IF period.subLedgerBR EQ "C" AND ipcModule EQ "BR" THEN DO:
          MESSAGE "Bank Reconciliation - Already Closed. " VIEW-AS ALERT-BOX ERROR.
          v-invalid = YES.
       END.
       ELSE IF period.subLedgerAR EQ "C" AND ipcModule EQ "AR" THEN DO:
          MESSAGE "A/R Receivables - Already Closed. " VIEW-AS ALERT-BOX ERROR.
          v-invalid = YES.
       END.
       /*else do:
         find first alt-period
             where alt-period.company             eq cocode
               and alt-period.pst - period.pend   eq 1
               and (alt-period.pnum - period.pnum eq 1     or
                    (alt-period.pnum              eq 1 and
                     period.pnum eq company.num-per))
               and alt-period.pstat               eq yes
           no-lock no-error.
         if not avail alt-period then do:
           MESSAGE "NEXT PERIOD NOT DEFINED.  MUST DEFINE NEXT PERIOD!!!"
               VIEW-AS ALERT-BOX ERROR.
           v-invalid = YES.
         end.
         /* CODE FOR VERIFYING CLOSE OF ALL PRIOR PERIODS */
         else do:
           find first alt-period where alt-period.company eq cocode
                                   AND alt-period.yr   EQ tran-year
                                   AND alt-period.pnum EQ tran-period
                                 no-lock no-error.
           if avail alt-period then fiscal-yr = alt-period.yr.
           find first alt-period where alt-period.company eq cocode
                    and (alt-period.yr     lt fiscal-yr or
                        (alt-period.yr    eq fiscal-yr and
                         alt-period.pnum  lt period.pnum))
                    and alt-period.pstat   eq yes
                    no-lock no-error.
           if avail alt-period then do:
             ASSIGN per-open   = alt-period.pnum
                    per-status = alt-period.pstat.
             MESSAGE "PRIOR MONTH(S) NOT CLOSED.  MUST CLOSE ALL PRIOR MONTHS!!!"
                   VIEW-AS ALERT-BOX ERROR.
             v-invalid = YES.
           end.
           ELSE
           if period.pnum eq 1 AND ip-oktogo then do:
             MESSAGE "YOU ARE ABOUT TO CLOSE PERIOD 1." skip(1)
                     "YOU MUST MAKE SURE THE PRIOR FISCAL YEAR END PROCEDURE HAS BEEN RUN!!!"
                     skip(2)
                     "Do You Want to Continue and Close the Month? " VIEW-AS ALERT-BOX BUTTON YES-NO
                     update choice .
           end.
         end.
       END.   */
       /*tran-period:SCREEN-VALUE = string(period.pnum).*/
    END.

    ELSE DO:
      message "No Defined Period Exists for" tran-period view-as alert-box error.
      v-invalid = yes.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close-month C-Win 
PROCEDURE close-month :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR li AS INT NO-UNDO.
   DEF VAR lv-rowid AS ROWID NO-UNDO.

   DEF BUFFER b-period FOR period.

   SESSION:SET-WAIT-STATE ("general").

   find first gl-ctrl where gl-ctrl.company eq cocode no-lock no-error.
   find first company where company.company eq cocode.
   find first b-racct
       where b-racct.company eq cocode
         and b-racct.actnum  eq gl-ctrl.ret
       no-lock no-error.
   if not avail b-racct then do on endkey undo, return:
      message "Unable to Find Retained Earnings Account from G/L Control File."
              VIEW-AS ALERT-BOX ERROR.
      return.
   end.

   find first b-cacct
       where b-cacct.company eq cocode
         and b-cacct.actnum  eq gl-ctrl.contra
       no-lock no-error.
   if not avail b-cacct then do on endkey undo, return:
      message "Unable to Find Profit Contra Account from G/L Control File." VIEW-AS ALERT-BOX ERROR.
      return.
   end.
           
   RUN GL_pCloseMonthModule(cocode, tran-year, uperiod, ipcModule). /* Company,Year,Period,Module*/ 
      
  
   SESSION:SET-WAIT-STATE ("").

  /* message "Current accounting period changed to " uperiod VIEW-AS ALERT-BOX.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close-test C-Win 
PROCEDURE close-test :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*ASSIGN v-msg1:HIDDEN IN FRAME {&FRAME-NAME} = NO
          v-msg2:HIDDEN = NO
       v-msg1:BGCOLOR = 4
          v-msg1 = "PROCESSING... PLEASE WAIT and DO NOT CANCEL OUT OF SCREEN!". 
   DISPLAY v-msg1 WITH FRAME {&FRAME-NAME}.  */

 for each glhist
       where glhist.company eq cocode
         and glhist.tr-date ge period.pst
         and glhist.tr-date le period.pend
         and glhist.period  eq uperiod
         AND glhist.posted  EQ NO
    :

    /*ASSIGN v-msg2 = jrnl + "Period: "  + 
           string(gltrans.period) + "Act: " + gltrans.actnum.
    DISP v-msg2 WITH FRAME {&FRAME-NAME}.*/
    .

END.

END.

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
  DISPLAY fiText tran-year tran-period tb_sub-report tb_close-sub 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-7 tb_sub-report tb_close-sub btn-ok btn-cancel 
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

  /* /*Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */
    */
     run custom/prntproc.p (list-name,INT(lv-font-no), lv-ornt). /* open file-name, title */ 

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
    run scr-rpt.w (list-name,c-win:title,INT(lv-font-no), lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/*DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .
 DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.

 RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

 OUTPUT STREAM excel TO VALUE(cFileName).
 excelheader = "Account,Description,Reason" .
 PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' skip. */

 
form account.actnum label "Account Number"
     account.dscr   label "Account Description"
     glhist.jrnl   label " Journal "
     glhist.tr-amt format "(>>>,>>>,>>>,>>9.99)" label "Transaction"
     open-amt       label "Account Balance"
    with frame r-mclo down width 132 no-box column 10 STREAM-IO.

 {sys/form/r-topw.f}

 {sys/inc/print1.i}
 {sys/inc/outprint.i VALUE(99)}

/* IF td-show-parm THEN RUN show-param.*/
 SESSION:SET-WAIT-STATE("general").
 ASSIGN uperiod = tran-period .
        
 find first period                   
        where period.company eq cocode
            AND period.yr   EQ tran-year
            AND period.pnum EQ tran-period
        no-lock no-error.


 str-tit  = coname + " - " + loname.
 str-tit2 = "Monthly Summary & Close A/P – Payables" .
 str-tit3 = "Period " + string(uperiod,"99") + " - " +
               string(period.pst) + " to " + string(period.pend).
 x = (112 - length(str-tit)) / 2.
 str-tit  = fill(" ",x) + str-tit .
 x = (114 - length(str-tit2)) / 2.
 str-tit2 = fill(" ",x) + str-tit2 .
 x = (132 - length(str-tit3)) / 2.
 str-tit3 = fill(" ",x) + str-tit3 .

   display str-tit3 format "x(130)" skip(1) with frame r-top.

   SESSION:SET-WAIT-STATE ("general").

   /*for each account where account.company eq cocode NO-LOCK :
       IF tb_sub-report AND account.inactive THEN do:
           PUT STREAM excel UNFORMATTED
               '"' account.actnum                   '",'
               '"' account.dscr                     '",'
               '"' "Inactive Account"               '",'
                       SKIP.
       END.
   END.  */

   /*PUT STREAM excel UNFORMATTED SKIP(1) .

   PUT STREAM excel UNFORMATTED
       '"' "Account "                 '",'
       '"' "TR No   "                 '",'
       '"' "Description"              '",'
       '"' "Journal"                  '",'
       '"' "Date"                     '",'
       '"' "Period"                   '",'
       '"' "Amount"                   '",'
       '"' "Reason  "                 '",'
       SKIP. */

   for each account where account.company eq cocode no-lock with frame r-mclo:
       
      /* IF tb_close-sub THEN DO:

       END.

       IF tb_invalid-period THEN DO:
          FOR EACH gltrans no-lock 
              where gltrans.company eq cocode
              and gltrans.actnum  eq account.actnum
              and gltrans.tr-date ge period.pst
              and gltrans.tr-date le period.pend
              and gltrans.period EQ 0 BREAK BY gltrans.actnum:
              
              PUT STREAM excel UNFORMATTED
                  '"' account.actnum                  '",'
                  '"' gltrans.trnum                   '",'
                  '"' account.dscr                    '",'
                  '"' gltrans.jrnl                    '",'
                  '"' gltrans.tr-date                 '",'
                  '"' gltrans.period                 '",'
                  '"' gltrans.tr-amt                  '",'
                  '"' "Invalid Period  "               '",'
                  SKIP.
          END.
       END. */
      
       /*IF tb_post-out-period THEN DO:
           FOR EACH gltrans no-lock 
              where gltrans.company eq cocode
              and gltrans.actnum  eq account.actnum
              and gltrans.tr-date LT period.pst
              and gltrans.tr-date GT period.pend
              and gltrans.period EQ uperiod BREAK BY gltrans.actnum:
              
              PUT STREAM excel UNFORMATTED
                  '"' account.actnum                  '",'
                  '"' gltrans.trnum                   '",'
                  '"' account.dscr                    '",'
                  '"' gltrans.jrnl                    '",'
                  '"' gltrans.tr-date                 '",'
                  '"' gltrans.period                 '",'
                  '"' gltrans.tr-amt                  '",'
                  '"' "Data outside period  "         '",'
                  SKIP.
          END.
       END.*/

       /*IF tb_prior-period-data THEN DO:
           FOR EACH gltrans no-lock 
              where gltrans.company eq cocode
              and gltrans.actnum  eq account.actnum
              and gltrans.tr-date LT period.pst :
              
              PUT STREAM excel UNFORMATTED
                  '"' account.actnum                  '",'
                  '"' gltrans.trnum                   '",'
                  '"' account.dscr                    '",'
                  '"' gltrans.jrnl                    '",'
                  '"' gltrans.tr-date                 '",'
                  '"' gltrans.period                 '",'
                  '"' gltrans.tr-amt                  '",'
                  '"' "Invalid Data  "  '",'
                  SKIP.
          END.
       END. */


      if line-counter gt page-size - 3 then page.
      open-amt = account.cyr-open.
      do i = 1 to uperiod:
         open-amt = open-amt + cyr[i].
      end.
      find first glhist
          where glhist.company eq cocode
            and glhist.actnum  eq account.actnum
            and glhist.tr-date ge period.pst
            and glhist.tr-date le period.pend
            and glhist.period  eq uperiod
            AND glhist.posted  EQ NO
          no-lock no-error.
      if open-amt eq 0 and not avail glhist then next.
      display account.actnum
              account.dscr
              open-amt.
      down.
      tot-all = tot-all + open-amt.

      for each glhist no-lock
          where glhist.company eq account.company
            and glhist.actnum  eq account.actnum
            and glhist.tr-date ge period.pst
            and glhist.tr-date le period.pend
            and glhist.period  eq uperiod
            AND glhist.posted  EQ NO
          break by glhist.jrnl with frame r-mclo:

           

         if line-counter gt page-size - 2 then page.

         assign
          tot-tx   = tot-tx   + tr-amt
          tot-all  = tot-all  + tr-amt
          tot-jrnl = tot-jrnl + tr-amt
          tot-act  = tot-act  + tr-amt.

         if last-of(glhist.jrnl) then do:
            display "" @ account.actnum
                    "" @ account.dscr
                    glhist.jrnl
                    tot-jrnl @ glhist.tr-amt
                   "" @ open-amt.
            tot-jrnl = 0.
            down.
         end.
      end. /* each glhist */

      display "" @ account.actnum
              "" @ account.dscr
              "" @ glhist.jrnl
              tot-act @ glhist.tr-amt
              (tot-act + open-amt) format "->>>,>>>,>>>,>>9.99" @ open-amt
              "*" with frame r-mclo.
      down 2.
      tot-act = 0.
   end. /* each account */

   display "" @ account.actnum
           "" @ account.dscr
           "TOTAL" @ glhist.jrnl
           tot-tx  @ glhist.tr-amt
           tot-all @ open-amt
           with frame r-mclo.    

 SESSION:SET-WAIT-STATE("").

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
           ELSE IF lv-field-hdl:TYPE = "Fill-in" THEN 
               assign parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:help + "," 
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-period C-Win 
PROCEDURE valid-period :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

