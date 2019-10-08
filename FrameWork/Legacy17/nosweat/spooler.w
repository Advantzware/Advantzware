&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: spool.w

  Description: Spool Requests

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 07.19.2013

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

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

DEFINE VARIABLE endTime AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES user-print

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH user-print SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH user-print SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME user-print
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME user-print


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 RECT-4 spool_list ~
Btn_Process Btn_Delete Btn_Close btnSpooler 
&Scoped-Define DISPLAYED-OBJECTS spool_list dayOfWeek-1 dayOfWeek-2 ~
dayOfWeek-3 dayOfWeek-4 dayOfWeek-5 dayOfWeek-6 dayOfWeek-7 repeatWeekly ~
spool_title program_id spool_date spool_time last_date last_time user_id ~
batchSeq start_date start_time end_date end_time 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 spool_list Btn_Process Btn_Delete Btn_Close ~
btnSpooler 
&Scoped-define List-2 intervalValue btnStartStop 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSpooler 
     LABEL "&Run Spool Monitor" 
     SIZE 21 BY 1.14.

DEFINE BUTTON btnStartStop 
     LABEL "&Start" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Close 
     LABEL "&Close" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Delete 
     LABEL "&Delete" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Process 
     LABEL "&Process" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE batchSeq AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Seq" 
      VIEW-AS TEXT 
     SIZE 16 BY .62
     BGCOLOR 15 .

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U 
     LABEL "End Date" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE end_time AS CHARACTER FORMAT "X(256)":U 
     LABEL "Time" 
      VIEW-AS TEXT 
     SIZE 13 BY .62
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE intervalValue AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "&Interval" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.14 NO-UNDO.

DEFINE VARIABLE last_date AS DATE FORMAT "99/99/9999":U 
     LABEL "Last Date" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE last_time AS CHARACTER FORMAT "X(256)":U 
     LABEL "Time" 
      VIEW-AS TEXT 
     SIZE 13 BY .62
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE program_id AS CHARACTER FORMAT "X(256)":U 
     LABEL "ID" 
      VIEW-AS TEXT 
     SIZE 45 BY .62
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE spool_date AS DATE FORMAT "99/99/9999":U 
     LABEL "Next Date" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE spool_time AS CHARACTER FORMAT "X(256)":U 
     LABEL "Time" 
      VIEW-AS TEXT 
     SIZE 13 BY .62
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE spool_title AS CHARACTER FORMAT "X(256)":U 
     LABEL "Title" 
      VIEW-AS TEXT 
     SIZE 45 BY .62
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE start_date AS DATE FORMAT "99/99/9999":U 
     LABEL "Start Date" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE start_time AS CHARACTER FORMAT "X(256)":U 
     LABEL "Time" 
      VIEW-AS TEXT 
     SIZE 13 BY .62
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE user_id AS CHARACTER FORMAT "X(256)":U 
     LABEL "User ID" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 13.81.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 5.48.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 3.1.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 4.76.

DEFINE VARIABLE spool_list AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 52 BY 12.95 NO-UNDO.

DEFINE VARIABLE dayOfWeek-1 AS LOGICAL INITIAL no 
     LABEL "S" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE dayOfWeek-2 AS LOGICAL INITIAL no 
     LABEL "M" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE dayOfWeek-3 AS LOGICAL INITIAL no 
     LABEL "T" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE dayOfWeek-4 AS LOGICAL INITIAL no 
     LABEL "W" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY .81 NO-UNDO.

DEFINE VARIABLE dayOfWeek-5 AS LOGICAL INITIAL no 
     LABEL "T" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE dayOfWeek-6 AS LOGICAL INITIAL no 
     LABEL "F" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE dayOfWeek-7 AS LOGICAL INITIAL no 
     LABEL "S" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE repeatWeekly AS LOGICAL INITIAL no 
     LABEL "Repeat Weekly" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      user-print SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     spool_list AT ROW 1.95 COL 2 HELP
          "Select Spool Request" NO-LABEL
     dayOfWeek-1 AT ROW 9.81 COL 71 WIDGET-ID 44
     dayOfWeek-2 AT ROW 9.81 COL 77 WIDGET-ID 46
     dayOfWeek-3 AT ROW 9.81 COL 83 WIDGET-ID 48
     dayOfWeek-4 AT ROW 9.81 COL 88 WIDGET-ID 50
     dayOfWeek-5 AT ROW 9.81 COL 94 WIDGET-ID 52
     dayOfWeek-6 AT ROW 9.81 COL 99 WIDGET-ID 54
     dayOfWeek-7 AT ROW 9.81 COL 104 WIDGET-ID 56
     repeatWeekly AT ROW 11 COL 71 WIDGET-ID 58
     Btn_Process AT ROW 12.43 COL 57 HELP
          "Process Spool Requests"
     Btn_Delete AT ROW 12.43 COL 73 HELP
          "DELETE Selected Spool Request"
     Btn_Close AT ROW 12.43 COL 94 HELP
          "CLOSE Spool Requests"
     intervalValue AT ROW 13.86 COL 63 COLON-ALIGNED HELP
          "Enter Interval in Minutes" WIDGET-ID 72
     btnStartStop AT ROW 13.86 COL 73 HELP
          "Start/Stop Spool Monitor" WIDGET-ID 70
     btnSpooler AT ROW 13.86 COL 88 HELP
          "Run this Process in Background Spool Monitor Mode" WIDGET-ID 68
     spool_title AT ROW 2.19 COL 62 COLON-ALIGNED
     program_id AT ROW 3.14 COL 62 COLON-ALIGNED WIDGET-ID 6
     spool_date AT ROW 4.1 COL 69 COLON-ALIGNED
     spool_time AT ROW 4.1 COL 94 COLON-ALIGNED
     last_date AT ROW 5.05 COL 69 COLON-ALIGNED WIDGET-ID 2
     last_time AT ROW 5.05 COL 94 COLON-ALIGNED WIDGET-ID 4
     user_id AT ROW 6 COL 69 COLON-ALIGNED
     batchSeq AT ROW 6 COL 91 COLON-ALIGNED WIDGET-ID 62
     start_date AT ROW 7.91 COL 69 COLON-ALIGNED WIDGET-ID 36
     start_time AT ROW 7.91 COL 94 COLON-ALIGNED WIDGET-ID 38
     end_date AT ROW 8.86 COL 69 COLON-ALIGNED WIDGET-ID 32
     end_time AT ROW 8.86 COL 94 COLON-ALIGNED WIDGET-ID 34
     " Spool Request Parameters" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 6.95 COL 58 WIDGET-ID 42
     "Day of Week:" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 9.81 COL 57 WIDGET-ID 60
     " Spool Request Detail" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.24 COL 58
     " Spool Requests" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1.24 COL 2
     RECT-1 AT ROW 1.48 COL 1
     RECT-2 AT ROW 1.48 COL 56
     RECT-3 AT ROW 12.19 COL 56
     RECT-4 AT ROW 7.19 COL 56 WIDGET-ID 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 109.2 BY 14.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Spool Requests"
         HEIGHT             = 14.33
         WIDTH              = 109.2
         MAX-HEIGHT         = 14.33
         MAX-WIDTH          = 109.2
         VIRTUAL-HEIGHT     = 14.33
         VIRTUAL-WIDTH      = 109.2
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN batchSeq IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnSpooler IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnStartStop IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
ASSIGN 
       btnStartStop:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON Btn_Close IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Delete IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Process IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX dayOfWeek-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX dayOfWeek-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX dayOfWeek-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX dayOfWeek-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX dayOfWeek-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX dayOfWeek-6 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX dayOfWeek-7 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN end_date IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN end_time IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN intervalValue IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE 2                                               */
ASSIGN 
       intervalValue:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN last_date IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN last_time IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN program_id IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX repeatWeekly IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN spool_date IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST spool_list IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN spool_time IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN spool_title IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN start_date IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN start_time IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN user_id IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "asi.user-print"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME




/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1
       COLUMN          = 21
       HEIGHT          = 4.76
       WIDTH           = 20
       WIDGET-ID       = 64
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Spool Requests */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Spool Requests */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSpooler
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSpooler C-Win
ON CHOOSE OF btnSpooler IN FRAME DEFAULT-FRAME /* Run Spool Monitor */
DO:
  DISABLE {&List-1} WITH FRAME {&FRAME-NAME}.
  ASSIGN
    btnStartStop:HIDDEN = NO
    intervalValue:HIDDEN = NO.
  ENABLE {&List-2} WITH FRAME {&FRAME-NAME}.
  APPLY 'ENTRY':U TO intervalValue.
  RETURN NO-APPLY.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStartStop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStartStop C-Win
ON CHOOSE OF btnStartStop IN FRAME DEFAULT-FRAME /* Start */
DO:
  IF intervalValue NE 0 THEN
  CASE SELF:LABEL:
    WHEN '~&Start' THEN DO:
      DISABLE intervalValue WITH FRAME {&FRAME-NAME}.
      ASSIGN
        SELF:LABEL = '~&Stop'
        chCtrlFrame:PSTimer:ENABLED = TRUE
        chCtrlFrame:PSTimer:Interval = 1000.
        endTime = 0.
      APPLY 'ENTRY':U TO SELF.
    END.
    WHEN '~&Stop' THEN DO:
      ASSIGN
        SELF:LABEL = '~&Start'
        chCtrlFrame:PSTimer:Interval = 0
        chCtrlFrame:PSTimer:ENABLED = FALSE
        intervalValue = 0
        intervalValue:HIDDEN = TRUE
        SELF:HIDDEN = TRUE.
      ENABLE {&List-1} WITH FRAME {&FRAME-NAME}.
    END.
  END CASE.
  ELSE DO:
    MESSAGE 'Please Enter Interval Time in Minutes' VIEW-AS ALERT-BOX WARNING.
    APPLY 'ENTRY':U TO intervalValue.
  END.
  RETURN NO-APPLY.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close C-Win
ON CHOOSE OF Btn_Close IN FRAME DEFAULT-FRAME /* Close */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Delete C-Win
ON CHOOSE OF Btn_Delete IN FRAME DEFAULT-FRAME /* Delete */
DO:
  MESSAGE "Delete '" + spool_title:SCREEN-VALUE + "'?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
          UPDATE OKdelete AS LOGICAL.
  IF NOT OKdelete THEN RETURN NO-APPLY.
  FIND FIRST user-print EXCLUSIVE-LOCK
       WHERE ROWID(user-print) EQ TO-ROWID(spool_list:SCREEN-VALUE) NO-ERROR.
  IF AVAILABLE user-print THEN DO:
    FIND FIRST user-batch EXCLUSIVE-LOCK
         WHERE user-batch.company EQ user-print.company
           AND user-batch.batch-seq EQ user-print.batch-seq
           AND user-batch.prog-seq EQ user-print.prog-seq NO-ERROR.
    IF AVAILABLE user-batch THEN
    DELETE user-batch.
    DELETE user-print.
  END. /* avail user-print */
  RUN getSpoolRequests.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Process C-Win
ON CHOOSE OF Btn_Process IN FRAME DEFAULT-FRAME /* Process */
DO:
  RUN runSpool.
  RUN getSpoolRequests.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
  IF btnStartStop:LABEL IN FRAME {&FRAME-NAME} EQ '~&Stop' THEN DO:
    IF TIME GE endTime THEN DO:
      RUN runSpool.
      endTime = TIME + intervalValue * 60.
      SESSION:SET-WAIT-STATE('').
    END. /* if time */
  END. /* if label */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME intervalValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL intervalValue C-Win
ON LEAVE OF intervalValue IN FRAME DEFAULT-FRAME /* Interval */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spool_list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spool_list C-Win
ON VALUE-CHANGED OF spool_list IN FRAME DEFAULT-FRAME
DO:
  RUN spoolDetails.
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
  RUN enable_UI.

  /* if blank, probably auto running, no company value exists at this point */
  IF g_company EQ '' THEN DO:
    g_company = '001'. /* set a default */
    FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.name EQ "Spooler" NO-ERROR.
    IF AVAILABLE sys-ctrl THEN
    g_company = sys-ctrl.char-fld.
  END. /* if g_company */

  RUN getSpoolRequests.

  IF INDEX(PROGRAM-NAME(2),'persist') EQ 0 AND
     AVAILABLE sys-ctrl AND sys-ctrl.log-fld EQ YES THEN DO:
    APPLY 'CHOOSE':U TO btnSpooler.
    intervalValue:SCREEN-VALUE = STRING(sys-ctrl.int-fld).
    APPLY 'LEAVE':U TO intervalValue.
    APPLY 'CHOOSE':U TO btnStartStop.
  END.

  SESSION:SET-WAIT-STATE('').

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "spooler.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "spooler.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  RUN control_load.
  DISPLAY spool_list dayOfWeek-1 dayOfWeek-2 dayOfWeek-3 dayOfWeek-4 dayOfWeek-5 
          dayOfWeek-6 dayOfWeek-7 repeatWeekly spool_title program_id spool_date 
          spool_time last_date last_time user_id batchSeq start_date start_time 
          end_date end_time 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 RECT-3 RECT-4 spool_list Btn_Process Btn_Delete 
         Btn_Close btnSpooler 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSpoolRequests C-Win 
PROCEDURE getSpoolRequests :
/*------------------------------------------------------------------------------
  Purpose:     Get Spool Requests
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE search-dir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE file-name AS CHARACTER FORMAT "X(26)" NO-UNDO.
  DEFINE VARIABLE attr-list AS CHARACTER FORMAT "X(4)" NO-UNDO.
  DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cdummy AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE ctitle AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      spool_list:LIST-ITEM-PAIRS = ?
      spool_title:SCREEN-VALUE = ""
      spool_date:SCREEN-VALUE = ""
      spool_time:SCREEN-VALUE = ""
      user_id:SCREEN-VALUE = ""
      batchSeq:SCREEN-VALUE = "".
  END.

  FOR EACH user-print NO-LOCK
      WHERE user-print.company EQ g_company
        AND user-print.batch NE '':
    spool_list:ADD-LAST(user-print.prog-title,STRING(ROWID(user-print))) IN FRAME {&FRAME-NAME}.
  END. /* each user-print */

  IF spool_list:NUM-ITEMS EQ 0 THEN DO:
    DISABLE {&LIST-1} WITH FRAME {&FRAME-NAME}.
    MESSAGE "No Spool Requests Exist!" VIEW-AS ALERT-BOX INFORMATION.
    RETURN.
  END.

  DO WITH FRAME {&FRAME-NAME}:
    spool_list:SCREEN-VALUE = spool_list:ENTRY(1).
    APPLY "VALUE-CHANGED" TO spool_list.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE runSpool C-Win 
PROCEDURE runSpool :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH user-print WHERE user-print.batch NE '',
      FIRST user-batch NO-LOCK
      WHERE user-batch.company EQ user-print.company
        AND user-batch.batch-seq EQ user-print.batch-seq
        AND user-batch.prog-seq EQ user-print.prog-seq:
    IF (TODAY GT user-print.next-date OR
       (TODAY EQ user-print.next-date AND
        TIME GE user-print.next-time)) AND
        SEARCH(user-print.program-id) NE ? THEN DO:
      OUTPUT TO 'spoolrpt/spooler.log' APPEND.
      RUN VALUE(user-print.program-id) (INPUT user-print.batch-seq).
      RUN setNextRun.
      PUT UNFORMATTED
        user-print.program-id AT 1
        user-print.last-date FORMAT '99.99.9999' AT 30 ' @ '
        STRING(user-print.last-time,'hh:mm:ss am') SKIP.
      OUTPUT CLOSE.
    END. /* run spool */
  END. /* each user-print */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setNextRun C-Win 
PROCEDURE setNextRun :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE firstDay AS INTEGER NO-UNDO.
  DEFINE VARIABLE nextDay AS INTEGER NO-UNDO.
  DEFINE VARIABLE day# AS INTEGER NO-UNDO.
  DEFINE VARIABLE idx AS INTEGER NO-UNDO.

  ASSIGN
    user-print.last-date = TODAY
    user-print.last-time = TIME
    /* assume no more runs to start */
    user-print.next-date = ?
    day# = WEEKDAY(TODAY).
  /* check if future runs required */
  IF user-batch.endDate GT TODAY OR
    (user-batch.endDate EQ TODAY AND
     user-batch.endTime GE TIME) THEN DO:
    DO idx = 1 TO 7:
      IF firstDay EQ 0 AND user-batch.dayOfWeek[idx] THEN
      firstDay = idx. /* first day of week selected */
      IF nextDay EQ 0 AND idx GT day# AND user-batch.dayOfWeek[idx] THEN
      nextDay = idx. /* next day of week selected */
    END. /* do idx */
    IF nextDay NE 0 THEN /* increment to next day of week */
    ASSIGN
      user-print.next-date = TODAY + (nextDay - day#)
      user-print.next-time = user-batch.startTime.
    ELSE
    /* if weekly repeat, loop around and set to first day of week */
    IF user-batch.repeatWeekly AND firstDay NE 0 THEN
    ASSIGN
      user-print.next-date = TODAY + (7 - day# + firstDay)
      user-print.next-time = user-batch.startTime.
  END. /* if lt today */
  /* failed to assign next date, so clear values, no more runs */
  IF user-print.next-date = ? THEN
  user-print.next-time = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spoolDetails C-Win 
PROCEDURE spoolDetails :
/*------------------------------------------------------------------------------
  Purpose:     Get Spool Details for selected spool request.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE spoolfile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ctitle AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cuserid AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cdate AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ctime AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ampm AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST user-print NO-LOCK
         WHERE ROWID(user-print) EQ TO-ROWID(spool_list:SCREEN-VALUE) NO-ERROR.
    IF AVAILABLE user-print THEN DO:
      FIND FIRST user-batch NO-LOCK
           WHERE user-batch.company EQ user-print.company
             AND user-batch.batch-seq EQ user-print.batch-seq
             AND user-batch.prog-seq EQ user-print.prog-seq NO-ERROR.
      IF AVAILABLE user-batch THEN
      ASSIGN
        spool_title:SCREEN-VALUE = user-print.prog-title
        program_id:SCREEN-VALUE = user-print.program-id
        spool_date:SCREEN-VALUE = STRING(user-print.next-date,'99.99.9999')
        spool_time:SCREEN-VALUE = STRING(user-print.next-time,'hh:mm am')
        last_date:SCREEN-VALUE = STRING(user-print.last-date,'99.99.9999')
        last_time:SCREEN-VALUE = STRING(user-print.last-time,'hh:mm am')
        user_id:SCREEN-VALUE = user-print.user-id
        batchSeq:SCREEN-VALUE = STRING(user-print.batch-seq)
        start_date:SCREEN-VALUE = STRING(user-batch.startDate,'99.99.9999')
        start_time:SCREEN-VALUE = STRING(user-batch.startTime,'hh:mm am')
        end_date:SCREEN-VALUE = STRING(user-batch.endDate,'99.99.9999')
        end_time:SCREEN-VALUE = STRING(user-batch.endTime,'hh:mm am')
        dayOfWeek-1:SCREEN-VALUE = STRING(user-batch.dayOfWeek[1])
        dayOfWeek-2:SCREEN-VALUE = STRING(user-batch.dayOfWeek[2])
        dayOfWeek-3:SCREEN-VALUE = STRING(user-batch.dayOfWeek[3])
        dayOfWeek-4:SCREEN-VALUE = STRING(user-batch.dayOfWeek[4])
        dayOfWeek-5:SCREEN-VALUE = STRING(user-batch.dayOfWeek[5])
        dayOfWeek-6:SCREEN-VALUE = STRING(user-batch.dayOfWeek[6])
        dayOfWeek-7:SCREEN-VALUE = STRING(user-batch.dayOfWeek[7])
        repeatWeekly:SCREEN-VALUE = STRING(user-batch.repeatWeekly)
        .
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

