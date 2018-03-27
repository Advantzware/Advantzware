&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util\openTransactions.w

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
DEFINE STREAM s1.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 fiFileName fiIterations fiPauseTime ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fiFileName fiIterations fiPauseTime 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE fiFileName AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\asi~\TransactionReport.txt" 
     LABEL "Output to File" 
     VIEW-AS FILL-IN 
     SIZE 57 BY 1 NO-UNDO.

DEFINE VARIABLE fiIterations AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 4 
     LABEL "# Of Times to Check for Transactions" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPauseTime AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 15 
     LABEL "Number of Minutes In Between Transaction Check" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 11.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fiFileName AT ROW 3.14 COL 16 COLON-ALIGNED WIDGET-ID 2
     fiIterations AT ROW 5.52 COL 52 COLON-ALIGNED WIDGET-ID 4
     fiPauseTime AT ROW 6.95 COL 52 COLON-ALIGNED WIDGET-ID 6
     btn-process AT ROW 15.29 COL 21
     btn-cancel AT ROW 15.29 COL 53
     RECT-17 AT ROW 2.19 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 17.52.


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
         TITLE              = "Report Open Transactions"
         HEIGHT             = 17.71
         WIDTH              = 90.4
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 142.4
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 142.4
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Report Open Transactions */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Report Open Transactions */
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


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  def var v-process as log no-undo.

  run run-process.
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
    /* check security */
  
    
  RUN enable_UI.
  
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
  DISPLAY fiFileName fiIterations fiPauseTime 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 fiFileName fiIterations fiPauseTime btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN {&displayed-objects}.
END.

SESSION:SET-WAIT-STATE ("general").

def var iNumTries as int.
def var iPauseLength as int.
def var iRepCount as int.
def var cOutputFile as char format "x(40)" init "c:\tmp\asi\transactionReport.txt".
    cOutPutFile = fiFileName.
    iNumTries = fiIterations.
    iPauseLength = fiPauseTime.
    
    iPauseLength = iPauseLength * 60.
    pause 0 before-hide.
    current-window:width-chars = 210.
    
    if cOutPutFile gt "" then 
      output STREAM s1 to value(cOutputfile).
    FIND FIRST asi._myconnection.
    iRepCount = 0.
    NEXT-I:
    repeat:
    
    iRepCount = iRepCount + 1.
    if iRepCount gt 1 then 
    pause iPauseLength.
    if iRepCount gt iNumTries + 1 then leave NEXT-I.
    
    FOR EACH asi._trans WHERE  .
    form
     asi._connect._connect-name
     asi._connect._connect-device
     asi._trans._trans-id 
     asi._trans._trans-num 
     asi._trans._trans-txtime 
     asi._trans._trans-duration 
     asi._trans._trans-usrnum
     asi._lock._lock-table
     asi._file._file-name
     with frame xy width 200 20 down stream-io.
     
      if asi._trans._trans-num eq 0 then leave.
      if asi._trans._trans-num eq ? then next.
      
      disp STREAM s1 asi._trans._trans-id asi._trans._trans-num asi._trans._trans-txtime 
           asi._trans._trans-duration asi._trans._trans-usrnum
           with frame xy
           . 
      find first asi._connect where asi._connect._connect-usr = asi._trans._trans-usrnum no-lock no-error.
      
      if avail asi._connect then 
         disp STREAM s1 asi._connect._connect-name asi._connect._connect-device
              with frame xy.
      
    
      
      for each asi._lock 
         where /* asi._lock._lock-usr = asi._connect._connect-usr  */
         no-lock:
         IF asi._Lock._Lock-Usr = ? THEN LEAVE.
         if asi._lock._lock-usr ne asi._connect._connect-usr then 
           next.
         /*if asi._lock._lock-id eq ? or asi._lock._lock-table eq ? then leave. */
    
        disp STREAM s1 asi._lock._lock-table with frame xy.
        
          if avail asi._lock then do:
            FIND asi._file WHERE asi._file._file-number = asi._lock._lock-table NO-LOCK NO-ERROR.
            disp STREAM s1 asi._file._file-name with frame xy.
          end.
        
      end.
    
      
    END.


FOR EACH nosweat._trans WHERE  .

form
 nosweat._connect._connect-name
 nosweat._connect._connect-device
 nosweat._trans._trans-id 
 nosweat._trans._trans-num 
 nosweat._trans._trans-txtime 
 nosweat._trans._trans-duration 
 nosweat._trans._trans-usrnum
 nosweat._lock._lock-table
 nosweat._file._file-name
 with frame xyz width 200 20 down stream-io.
 
  if nosweat._trans._trans-num eq 0 then leave.
  if nosweat._trans._trans-num eq ? then next.
  
  disp nosweat._trans._trans-id
       nosweat._trans._trans-num
       nosweat._trans._trans-txtime
       nosweat._trans._trans-duration
       nosweat._trans._trans-usrnum
       with frame xyz.
       
    find first nosweat._connect where nosweat._connect._connect-usr = nosweat._trans._trans-usrnum no-lock no-error.
  
  if avail nosweat._connect then 
    disp STREAM s1  nosweat._connect._connect-name nosweat._connect._connect-device with frame xyz .
 

  
  for each nosweat._lock 
     where /* nosweat._lock._lock-usr = nosweat._connect._connect-usr  */
     no-lock:
     IF nosweat._Lock._Lock-Usr = ? THEN LEAVE.
     if nosweat._lock._lock-usr ne nosweat._connect._connect-usr then 
       next.
     /*if nosweat._lock._lock-id eq ? or nosweat._lock._lock-table eq ? then leave. */
    disp STREAM s1 nosweat._lock._lock-table with frame xyz.
      if avail nosweat._lock then do:
        FIND nosweat._file WHERE nosweat._file._file-number = nosweat._lock._lock-table NO-LOCK NO-ERROR.
        disp STREAM s1 nosweat._file._file-name with frame xyz.
      end.
    
  end.

  
END.

if connected("emptrack") then do:
FOR EACH emptrack._trans WHERE  .

form
emptrack._connect._connect-name
emptrack._connect._connect-device
emptrack._trans._trans-id 
emptrack._trans._trans-num 
emptrack._trans._trans-txtime 
emptrack._trans._trans-duration 
emptrack._trans._trans-usrnum
emptrack._lock._lock-table
emptrack._file._file-name
 with frame xyzz width 200 20 down stream-io.
 
  if emptrack._trans._trans-num eq 0 then leave.
  if emptrack._trans._trans-num eq ? then next.
  
  disp STREAM s1 emptrack._trans._trans-id emptrack._trans._trans-num
       emptrack._trans._trans-txtime emptrack._trans._trans-duration
       emptrack._trans._trans-usrnum with frame xyzz. 
    find first emptrack._connect where emptrack._connect._connect-usr = emptrack._trans._trans-usrnum no-lock no-error.
  
  if avail emptrack._connect then 
    disp STREAM s1 emptrack._connect._connect-name emptrack._connect._connect-device with frame xyzz .
 

  
  for each emptrack._lock 
     where /* emptrack._lock._lock-usr = emptrack._connect._connect-usr  */
     no-lock:
     IF emptrack._Lock._Lock-Usr = ? THEN LEAVE.
     if emptrack._lock._lock-usr ne emptrack._connect._connect-usr then 
       next.
     /*if emptrack._lock._lock-id eq ? or emptrack._lock._lock-table eq ? then leave. */
    disp STREAM s1 emptrack._lock._lock-table with frame xyzz.
      if avail emptrack._lock then do:
        FIND emptrack._file WHERE emptrack._file._file-number = emptrack._lock._lock-table NO-LOCK NO-ERROR.
        disp STREAM s1 emptrack._file._file-name with frame xyzz.
      end.
    
  end.

  
END.
end. /* emptrack */
end. /* repeat */

OUTPUT STREAM s1 close.


SESSION:SET-WAIT-STATE ("").

message trim(c-win:title) + " Process Is Completed." view-as alert-box.
apply "close" to this-procedure.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

